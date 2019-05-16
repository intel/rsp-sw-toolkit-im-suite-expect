/*
 * INTEL CONFIDENTIAL
 * Copyright (2018) Intel Corporation.
 *
 * The source code contained or described herein and all documents related to the source code ("Material")
 * are owned by Intel Corporation or its suppliers or licensors. Title to the Material remains with
 * Intel Corporation or its suppliers and licensors. The Material may contain trade secrets and proprietary
 * and confidential information of Intel Corporation and its suppliers and licensors, and is protected by
 * worldwide copyright and trade secret laws and treaty provisions. No part of the Material may be used,
 * copied, reproduced, modified, published, uploaded, posted, transmitted, distributed, or disclosed in
 * any way without Intel/'s prior express written permission.
 * No license under any patent, copyright, trade secret or other intellectual property right is granted
 * to or conferred upon you by disclosure or delivery of the Materials, either expressly, by implication,
 * inducement, estoppel or otherwise. Any license under such intellectual property rights must be express
 * and approved by Intel in writing.
 * Unless otherwise agreed by Intel in writing, you may not remove or alter this notice or any other
 * notice embedded in Materials by Intel or Intel's suppliers or licensors in any way.
 */

// Package expect makes tests' expectations more obvious.
package expect

import (
	"errors"
	"fmt"
	"reflect"
	"strings"
	"testing"
)

// TWrapper wraps a *testing.T, allowing fluent asserts with clear error messages.
// Any *testing.T methods, such as t.Log, can be used as normal.
type TWrapper struct {
	onFail failFunc
	testing.TB
}

// WrapT returns a TWrapper, which allows several fluent assertions while testing.
// WrapT can also wrap a *testing.B, if desired.
func WrapT(t testing.TB) *TWrapper {
	return (&TWrapper{TB: t}).ContinueOnMismatch()
}

type failFunc = func(fmt string, args ...interface{})

func (t *TWrapper) dup(ff failFunc) *TWrapper {
	return &TWrapper{
		TB:     t.TB,
		onFail: ff,
	}
}

// As adds d to the message if an expectation fails. Use it with statements like:
//     w.As("empty json input").ShouldFail(validateJSON(""))
func (t *TWrapper) As(d interface{}) *TWrapper {
	t.Helper()
	t2 := t.dup(func(format string, args ...interface{}) {
		t.Helper()
		t.onFail(fmt.Sprintf("Failure for '%+v': %s", d, format), args...)
	})
	return t2
}

// StopOnMismatch returns a TWrapper that stop testing as soon as an expectation fails.
func (t *TWrapper) StopOnMismatch() *TWrapper {
	t.Helper()
	return t.dup(func(fmt string, args ...interface{}) {
		t.Helper()
		t.Fatalf(fmt, args...)
	})
}

// ContinueOnMismatch returns a TWrapper that continues testing, even if an expectation fails.
func (t *TWrapper) ContinueOnMismatch() *TWrapper {
	t.Helper()
	return t.dup(func(fmt string, args ...interface{}) {
		t.Helper()
		t.Errorf(fmt, args...)
	})
}

// ShouldBeEqual expects x and y to be equal, as compared by reflect.DeepEqual.
//
// See the docs for DeepEqual for all the rules, but in particular note that it
// returns true only if both parameters are the same type, and some types are
// inherently incomparable (e.g., non-nil func types are always unequal, even
// when compared to themselves), but pointers to such types may be compared and
// are equal when pointing to the same value.
//
// As a special case, unequal strings and byte slices are printed in such a way
// as to highlight their first difference.
func (t *TWrapper) ShouldBeEqual(x, y interface{}) {
	t.Helper()
	if reflect.DeepEqual(x, y) {
		return
	}

	if reflect.TypeOf(x) == reflect.TypeOf(y) &&
		isStringOrByteSlice(x) && isStringOrByteSlice(y) {
		var b1, b2 []byte
		b1, ok := x.([]byte)
		if ok {
			b2 = y.([]byte)
		} else {
			b1 = []byte(x.(string))
			b2 = []byte(y.(string))
		}
		idx := firstDiff(b1, b2)
		t.onFail(""+
			"byte-level differences:\n"+
			"    %[3]*sv--first diff at idx %[3]d\n"+
			"S1: %[1]s\n"+
			"S2: %[2]s\n"+
			"    %[3]*s^--first diff at idx %[3]d",
			b1, b2, idx, "")
		return
	}

	t.onFail("%+v != %+v (types %T, %T)", x, y, x, y)
}

// firstDiff returns smallest index i such that b1[i] != b2[i]; if b1 == b2, then
// the return value is their length.
func firstDiff(b1, b2 []byte) int {
	smaller := len(b2)
	if len(b1) < len(b2) {
		smaller = len(b1)
	}
	for i := 0; i < smaller; i++ {
		if b1[i] != b2[i] {
			return i
		}
	}
	return smaller
}

// Iterator types can be used for containment comparisons.
type Iterator interface {
	// Next advances the iterator and returns true if there are more elements.
	// If Next returns true, Value must return an element and not panic.
	// Next must be called before using an iterator, and the first call should
	// never panic. When the iterator is exhausted, Next returns false, and subsequent
	// calls to Next or Value will panic, unless Reset is used to reset the iterator.
	Next() bool
	// Value returns the current iterator element.
	// It will panic if the iterator is exhausted.
	Value() (v reflect.Value)
	// Reset resets the iterator to its initial value.
	// Resetting an iterator that's 'empty' (in the sense that Next will never
	// return true) should not result in a panic.
	Reset()
}

// Iterable types can return an iterator.
type Iterable interface {
	// GetIterator should return a unique Iterator, independent of any other
	// Iterator operating on the same Iterable.
	GetIterator() Iterator
}

type listIter struct {
	idx, len int
	list     reflect.Value
}

func (li *listIter) Next() bool {
	if li.idx == li.len {
		panic("Next called on exhausted list iterator")
	}
	li.idx += 1
	return li.idx < li.len
}

func (li *listIter) Value() reflect.Value {
	if li.idx == -1 {
		panic("Value called before next")
	}
	if li.idx == li.len {
		panic("Value called on exhausted list iterator")
	}
	v := li.list.Index(li.idx)
	return v
}

func (li *listIter) Reset() {
	li.idx = -1
}

type mapKeyIter struct {
	// *reflect.MapIter - added in 1.12, so not usable for us right now :(
	uMap reflect.Value // underlying map
	keys *listIter
}

func (mki *mapKeyIter) Reset() {
	mki.keys.Reset()
}
func (mki *mapKeyIter) Next() bool {
	return mki.keys.Next()
}
func (mki *mapKeyIter) Value() reflect.Value {
	return mki.keys.Value().Interface().(reflect.Value)
}

type mapValIter mapKeyIter

func (mi *mapValIter) Value() reflect.Value {
	k := mi.keys.Value()
	return mi.uMap.MapIndex(k)
}
func (mi *mapValIter) Reset() {
	((*mapKeyIter)(mi)).Reset()
}
func (mi *mapValIter) Next() bool {
	return ((*mapKeyIter)(mi)).Next()
}

// NewIterator returns an Iterator for a slice, array, string, or map, or a type
// that implements the Iterable or Iterator interfaces.
//
// Other types return a nil iterator and an error. Map types return an iterator
// over its keys; to obtain an iterator over its values, use NewValueIterator.
func NewIterator(i interface{}) (Iterator, error) {
	if it, ok := i.(Iterator); ok {
		return it, nil
	}
	if it, ok := i.(Iterable); ok {
		return it.GetIterator(), nil
	}

	v := reflect.ValueOf(i)
	switch v.Kind() {
	case reflect.Slice, reflect.Array, reflect.String:
		return &listIter{len: v.Len(), list: v, idx: -1}, nil
	case reflect.Map:
		li, err := NewIterator(v.MapKeys())
		if err != nil {
			return nil, err
		}
		return &mapKeyIter{keys: li.(*listIter), uMap: v}, nil
	}

	return nil, errors.New("non-iterable type")
}

// NewValueIterator returns an Iterator over the Values of a Map, rather than
// the keys, as is the case for NewIterator.
//
// Non-map types return nil and an error.
func NewValueIterator(i interface{}) (Iterator, error) {
	v := reflect.ValueOf(i)
	if v.Kind() != reflect.Map {
		return nil, errors.New("can only create value iterators for maps")
	}
	it, err := NewIterator(i)
	if err != nil {
		return nil, err
	}
	return (*mapValIter)(it.(*mapKeyIter)), nil
}

// contains returns true if v is any element of the given iterator.
func contains(iter Iterator, v reflect.Value) bool {
	toFind := v.Interface()
	iter.Reset()
	for iter.Next() {
		if reflect.DeepEqual(iter.Value().Interface(), toFind) {
			return true
		}
	}
	return false
}

// ShouldContain checks that the container holds at least one instance of toFind,
// or at least one instance of every element of toFind, if toFind is made up of
// elements.
//
// container must be a slice, array, string, map, iterable, or iterator, or this
// method will panic. toFind may be of any type, but if it is also a slice, array,
// string, map, iterable, or iterator, then container is searched for individual
// elements of toFind and passes if and only if it contains at least one instance
// of each element. The element order is not considered, nor are duplicate
// instances distinguished.
//
// By default maps iterate on their keys; to use values instead, wrap it in
// NewValueIterator. You may implement your own Iterator or Iterable instances
// on types if you'd like fine-grained control over the elements under consideration.
//
// For example, the following cases will pass:
//   ShouldContain([]int{1,2,3,4,5}, 1)
//   ShouldContain([]int{1,2,3,4,5}, []int{5,3})
//   ShouldContain([]string{"a","b","c","d"}, map[string]int{"a":1})
//   ShouldContain(map[int]int{1:6,2:7,3:8,4:9,5:10}, []int{5,3})
//   ShouldContain([]string{"a","b","c","d"}, "abc")
//   ShouldContain("cbad", "abc")
func (t *TWrapper) ShouldContain(container, toFind interface{}) {
	t.Helper()
	conIter, err := NewIterator(container)
	if err != nil {
		t.onFail("type %T for %+v is not iterable, so can't check if it contains %+v (type %T)",
			container, container, toFind, toFind)
	}

	fIter, _ := NewIterator(toFind)
	if fIter == nil {
		if !contains(conIter, reflect.ValueOf(toFind)) {
			t.onFail("%+v (type %T) does not contain %+v (type %T)",
				container, container, toFind, toFind)
		}
		return
	}

	for fIter.Next() {
		toFind := fIter.Value()
		if !contains(conIter, toFind) {
			t.onFail("%+v (type %T) does not contain element %+v (kind %s) of iterable",
				container, container, toFind, toFind.Kind())
		}
	}
}

func isStringOrByteSlice(x interface{}) bool {
	xt := reflect.TypeOf(x)
	return xt.Kind() == reflect.String ||
		((xt.Kind() == reflect.Slice || xt.Kind() == reflect.Array) &&
			xt.Elem().Kind() == reflect.Uint8)
}

// ShouldNotBeEqual expects x and y to be unequal as compared by reflect.DeepEqual.
func (t *TWrapper) ShouldNotBeEqual(x, y interface{}) {
	t.Helper()
	if !reflect.DeepEqual(x, y) {
		return
	}

	t.onFail("expected unequal, but were both: %+v", x, y)
}

// ShouldBeSameType expects x and y to be the same type.
func (t *TWrapper) ShouldBeSameType(x, y interface{}) {
	t.Helper()
	if reflect.TypeOf(x) != reflect.TypeOf(y) {
		t.onFail("type mismatch: (%T, %T) with values (%v, %v)",
			x, y, x, y)
	}
}

// ShouldBeFalse expects v to be false.
func (t *TWrapper) ShouldBeFalse(v interface{}) {
	t.Helper()
	t.ShouldBeEqual(false, v)
}

// ShouldBeTrue expects v to be true.
func (t *TWrapper) ShouldBeTrue(v interface{}) {
	t.Helper()
	t.ShouldBeEqual(true, v)
}

func isNil(v interface{}) bool {
	switch reflect.ValueOf(v).Kind() {
	case reflect.Chan, reflect.Func, reflect.Map, reflect.Ptr, reflect.Interface, reflect.Slice:
		return v == nil || reflect.ValueOf(v).IsNil()
	default:
		return v == nil || reflect.TypeOf(v) == nil
	}
}

// ShouldNotBeNil expect v to not be nil.
func (t *TWrapper) ShouldNotBeNil(v interface{}) {
	t.Helper()
	if !isNil(v) {
		return
	}
	t.onFail("value is nil, but shouldn't be (type %T)", v)
}

// ShouldBeNil expect v to be nil.
func (t *TWrapper) ShouldBeNil(v interface{}) {
	t.Helper()
	if isNil(v) {
		return
	}
	t.onFail("expected value to be nil, but it's %+v (type %T)", v, v)
}

// ShouldBeEmptyStr expects v to be an empty string.
func (t *TWrapper) ShouldBeEmptyStr(v interface{}) {
	t.Helper()
	t.ShouldBeEqual("", v)
}

// ShouldNotBeEmptyStr expect v not to be an empty string.
func (t *TWrapper) ShouldNotBeEmptyStr(v interface{}) {
	t.Helper()
	t.ShouldNotBeEqual("", v)
}

// ShouldContainStr expects string x to contain y.
func (t *TWrapper) ShouldContainStr(x, y string) {
	t.Helper()
	t.As(fmt.Sprintf("'%s' should contain '%s'", x, y)).ShouldBeTrue(strings.Contains(x, y))
}

// ShouldFail expects a given err to not be nil; it can be used with the immediate
// result of a function, such as w.ShouldFail(funcThatReturnsError()).
func (t *TWrapper) ShouldFail(err error) error {
	t.Helper()
	if err == nil {
		t.onFail("expected an error, but none occurred")
	}
	return err
}

// ShouldSucceed expects a given err to be nil; it can be used with the immediate
// result of a function, such as w.ShouldSucceed(funcThatReturnsError()).
func (t *TWrapper) ShouldSucceed(err error) {
	t.Helper()
	if err != nil {
		t.onFail("an unexpected error occurred: %+v", err)
	}
}

// ShouldSucceedLater takes a function, which it calls using ShouldSucceed.
// It's meant to be used for deferred functions, such as:
//     defer w.ShouldSucceedLater(c.Close)
// which will defer w.ShouldSucceed(c.Close()), ensuring that c.Close() succeeds.
func (t *TWrapper) ShouldSucceedLater(f func() error) {
	t.Helper()
	t.ShouldSucceed(f())
}

// ShouldHaveError takes a result interface and error and expects that the error
// is not nil; it can be used with the immediate result of a call that's expected
// to fail, but has a return value:
//     w.ShouldHaveError(os.Open("missingFile"))
func (t *TWrapper) ShouldHaveError(result interface{}, err error) error {
	t.Helper()
	if err == nil {
		t.onFail("expected an error, but none occurred")
	}

	if result != nil {
		v := reflect.ValueOf(result)
		if v.Kind() != reflect.Ptr || !v.IsNil() {
			t.Logf("function returned result: %+v", result)
		}
	}
	return err
}

// ShouldHaveResult takes a result interface and an error and expects that the
// error is not nil, in which case it returns the result, unmodified. This allows
// single-line error validation on function calls that return a type and an error.
// Note that, because the result must be wrapped in an interface, you'll have to
// type assert it before using it:
//    f := w.ShouldHaveResult(os.Open("myFile")).(*os.File)
func (t *TWrapper) ShouldHaveResult(result interface{}, err error) interface{} {
	t.Helper()
	if err != nil {
		t.StopOnMismatch().onFail("an unexpected error occurred: %+v", err)
	}
	return result
}

// ShouldHaveLength expects the given interface to have a particular length.
// The input can be an Array, Channel, Map, Slice, or String; if it isn't, then
// the check automatically fails.
func (t *TWrapper) ShouldHaveLength(itemWithLen interface{}, length int) {
	t.Helper()
	v := reflect.ValueOf(itemWithLen)
	switch v.Kind() {
	case reflect.Array, reflect.Chan, reflect.Map, reflect.Slice, reflect.String:
		if v.Len() == length {
			break
		}
		t.onFail("expected length %d, but actual is %d (type: %T, value: %+v)",
			length, v.Len(), itemWithLen, itemWithLen)
	default:
		t.onFail("expected array, channel, map, slice, or string, but was %T (%+v)",
			itemWithLen, v.Kind())
	}
}
