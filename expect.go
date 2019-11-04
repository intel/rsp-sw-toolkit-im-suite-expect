/* Apache v2 license
 * Copyright (C) <2019> Intel Corporation
 *
 * SPDX-License-Identifier: Apache-2.0
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

// As adds the obj's Go representation (using the verb %+v) if an expectation
// fails. Use it with statements like:
//     w.As("empty json input").ShouldFail(validateJSON(""))
// or
//     w.As(myType{f1: "value"}).Should...
// On failure, prints messages like:
//     "Failure for 'empty json input': expected an error, but none occurred.
// or
//     "Failure for '{f1: "value"}': expected an error, but none occurred.
func (t *TWrapper) As(obj interface{}) *TWrapper {
	t.Helper()
	t2 := t.dup(func(failFmt string, failArgs ...interface{}) {
		t.Helper()
		t.onFail(fmt.Sprintf("Failure for '%+v': %s", obj, failFmt), failArgs...)
	})
	return t2
}

// Asf works like As, but uses a format string and arguments instead printing
// the input with %+v.
//
// fmt.Sprintf is only called if an expectation fails, so this can speed up a
// test that loops over a large number of values and uses `As(fmt.Sprint(...))`.
func (t *TWrapper) Asf(format string, args ...interface{}) *TWrapper {
	t.Helper()
	t2 := t.dup(func(failFmt string, failArgs ...interface{}) {
		t.Helper()
		t.onFail(fmt.Sprintf("Failure for '%s': %s",
			fmt.Sprintf(format, args...), failFmt), failArgs...)
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

func equalIgnoringType(x, y interface{}) bool {
	if (x == nil || y == nil) && (x == nil && y == nil) {
		return false
	}

	xVal := reflect.ValueOf(x)
	yVal := reflect.ValueOf(y)
	xType := reflect.TypeOf(x)
	yType := reflect.TypeOf(y)

	switch {
	case !xVal.IsValid() || !yVal.IsValid():
		return false
	case xType.ConvertibleTo(yType):
		return reflect.DeepEqual(y, xVal.Convert(yType).Interface())
	case yType.ConvertibleTo(xType):
		return reflect.DeepEqual(x, yVal.Convert(xType).Interface())
	default:
		return false
	}
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

	// if they have different types, but one can be converted to the other,
	// see if they're equal when converted to the same type
	if equalIgnoringType(x, y) {
		if stringish(x) || stringish(y) {
			t.onFail("byte values are the same, but they have different types: %T != %T", x, y)
		} else {
			t.onFail("%+v == %+v, but they have different types: %T != %T", x, y, x, y)
		}
		return
	}

	if stringish(x) && stringish(y) {
		b1 := reflect.ValueOf(x).Convert(reflect.TypeOf([]byte{})).Interface().([]byte)
		b2 := reflect.ValueOf(y).Convert(reflect.TypeOf([]byte{})).Interface().([]byte)
		idx := firstDiff(b1, b2)

		t.onFail("\n"+
			"byte-level differences:\n"+
			"      %[3]*[4]sv--first diff at idx %[5]d\n"+
			"S1: %03[1]d\n"+
			"S2: %03[2]d\n"+
			"      %[3]*[4]s^--first diff at idx %[5]d\n"+
			"string-level differences:\n\n"+
			"    %[5]*[4]sv--first diff at idx %[5]d\n"+
			"S1: %[1]s\n"+
			"S2: %[2]s\n"+
			"    %[5]*[4]s^--first diff at idx %[5]d",
			b1, b2, idx*4, "", idx)
		return
	}

	t.onFail("%+v != %+v (types %T, %T)", x, y, x, y)
}

// firstDiff returns smallest index i such that b1[i] != b2[i].
// If b1 == b2, then the return value is their lengths.
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
// See GetIterator for more information.
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

// RuneIterable implements an iterator that returns runes for a string.
type RuneIterable string

func (r RuneIterable) GetIterator() Iterator {
	return &listIter{
		idx: 0, len: len(r), idxFunc: func(i int) reflect.Value { return reflect.ValueOf(r[i]) },
	}
}

type listIter struct {
	idx, len int
	idxFunc  func(int) reflect.Value
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
	return li.idxFunc(li.idx)
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

type mapValIter struct {
	*reflect.MapIter
	uMap reflect.Value
}

func (mi *mapValIter) Reset() {
	mi.MapIter = mi.uMap.MapRange()
}

// NewIterator returns an Iterator for a slice, array, string, or map, or a type
// that implements the Iterable or Iterator interfaces.
//
// Other types return a nil iterator and an error. Map types return an iterator
// over their keys; to obtain an iterator over its values, use NewValueIterator.
//
// By default, strings iterate over their bytes. To instead iterate over its
// runes, cast it using RuneIterable.
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
		return &listIter{len: v.Len(), idxFunc: v.Index, idx: -1}, nil
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
	return &mapValIter{v.MapRange(), v}, nil
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

// ShouldContainValues confirms the items in toFind are values of the map container.
// It fails if containerMap is not actually a map.
func (t *TWrapper) ShouldContainValues(containerMap, toFind interface{}) {
	t.Helper()
	it := t.ShouldHaveResult(NewValueIterator(containerMap)).(Iterator)
	t.ShouldContain(it, toFind)
}

// ShouldContainValuesFrom confirms toFind is a map and its values are in the container.
// It fails if toFindMap is not actually a map.
func (t *TWrapper) ShouldContainValuesFrom(container, toFindMap interface{}) {
	t.Helper()
	it := t.ShouldHaveResult(NewValueIterator(toFindMap)).(Iterator)
	t.ShouldContain(container, it)
}

// ShouldHaveMatchingValues confirms both inputs are maps and that all the values
// in toFindMap appear as values in the containerMap.
//
// It fails if either input is not actually a map.
func (t *TWrapper) ShouldHaveMatchingValues(containerMap, toFindMap interface{}) {
	t.Helper()
	cIt := t.ShouldHaveResult(NewValueIterator(containerMap)).(Iterator)
	fIt := t.ShouldHaveResult(NewValueIterator(toFindMap)).(Iterator)
	t.ShouldContain(cIt, fIt)
}

// ShouldHaveOrder confirms two iterable sequences have the same items in the
// same order.
//
// It advances both iterables simultaneously and only succeeds if the values
// returned by both are equal. If one iterator returns more elements than the
// other, then it fails.
func (t *TWrapper) ShouldHaveOrder(x, y interface{}) {
	t.Helper()
	it1, err := NewIterator(x)
	if err != nil {
		t.onFail("type %T for %+v is not iterable, so can't check if it matches %+v (type %T)",
			x, x, y, y)
	}

	it2, err := NewIterator(y)
	if err != nil {
		t.onFail("type %T for %+v is not iterable, so can't check if it matches %+v (type %T)",
			y, y, x, x)
		return
	}

	for {
		it1HasNext := it1.Next()
		it2HasNext := it2.Next()
		if (it1HasNext && !it2HasNext) || (!it1HasNext && it2HasNext) {
			t.onFail("sequences have different lengths\nx: type %T: %+v\ny: type %T: %+v",
				x, x, y, y)
			return
		}
		t.ShouldBeEqual(it1.Value().Interface(), it2.Value().Interface())
	}
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

// stringish returns true if the interface x can be interpreted as a string.
func stringish(x interface{}) bool {
	if x == nil {
		return false
	}
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

// ShouldNotBeEmptyStr expects v not to be an empty string.
func (t *TWrapper) ShouldNotBeEmptyStr(v interface{}) {
	t.Helper()
	t.ShouldNotBeEqual("", v)
}

// ShouldBeEmpty passes if v is an empty string, array, slice, map, or channel;
// if it's any other type, it fails regardless its value.
func (t *TWrapper) ShouldBeEmpty(v interface{}) {
	t.Helper()
	if v == nil {
		t.onFail("expected value to be empty, but it's nil (type %T)", v, v)
		return
	}
	rv := reflect.ValueOf(v)
	switch rv.Kind() {
	default:
		t.onFail("expected value with a length, but it's %+v (type %T)", v, v)
		return
	case reflect.Array, reflect.Slice, reflect.String, reflect.Map, reflect.Chan:
		if rv.Len() == 0 {
			return
		}
	}
	t.onFail("expected value to be empty, but it's %+v (type %T)", v, v)
}

// ShouldNotBeEmpty fails if v is an empty string, array, slice, map, or channel;
// if it's any other type, it fails regardless its value.
func (t *TWrapper) ShouldNotBeEmpty(v interface{}) {
	t.Helper()
	if v == nil {
		t.onFail("expected non-empty value, but %+v is nil (type %T)", v, v)
		return
	}
	rv := reflect.ValueOf(v)
	switch rv.Kind() {
	default:
		t.onFail("expected value with a length, but it's %+v (type %T)", v, v)
		return
	case reflect.Array, reflect.Slice, reflect.String, reflect.Map, reflect.Chan:
		if rv.Len() != 0 {
			return
		}
	}
	t.onFail("expected value to not be empty, but it's %+v (type %T)", v, v)
}

// ShouldContainStr expects string x to contain y.
func (t *TWrapper) ShouldContainStr(x, y string) {
	t.Helper()
	t.As(fmt.Sprintf("'%s' should contain '%s'", x, y)).ShouldBeTrue(strings.Contains(x, y))
}

// ShouldFail fails if none of its args are a non-nil error.
//
// It can be used with the immediate result of a function: w.ShouldFail(f()).
// If the function doesn't return any error types, it will always fail.
func (t *TWrapper) ShouldFail(args ...interface{}) {
	t.Helper()

	var errs []error
	for _, arg := range args {
		if err, ok := arg.(error); ok && err != nil {
			errs = append(errs, err)
		}
	}

	if len(errs) == 0 {
		t.onFail("expected an error, but none occurred")
	}
}

// ShouldSucceed fails if any of its args have type error and are non-nil.
//
// It can be used with the immediate result of a function: w.ShouldSucceed(f()).
// If the function doesn't return any error types, it will always succeed.
//
// This can also be used to verify the results of multiple functions that each
// return a single error: w.ShouldSucceed(f1(), f2(), f3()). Unfortunately, Go
// won't allow you to pass in multiple functions if one or more of them return
// more than one result.
func (t *TWrapper) ShouldSucceed(args ...interface{}) {
	t.Helper()
	var errs []error
	for _, arg := range args {
		if err, ok := arg.(error); ok && err != nil {
			errs = append(errs, err)
		}
	}
	if len(errs) > 0 {
		t.onFail("one or more unexpected errors occurred: %+v", errs)
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
//
// If the error is not nil, but the result is a non-nil, non-empty, non-zero
// instance of the underlying interface type, this method logs that result, but
// doesn't fail the test.
func (t *TWrapper) ShouldHaveError(result interface{}, err error) error {
	t.Helper()
	if err == nil {
		t.onFail("expected an error, but none occurred")
	}

	v := reflect.ValueOf(result)

	switch v.Kind() {
	default:
		if v.Interface() != reflect.Zero(v.Type()).Interface() {
			t.Logf("function returned result: %+v", result)
		}
	case reflect.Invalid:
	case reflect.Ptr, reflect.Func, reflect.Interface, reflect.UnsafePointer:
		if !v.IsNil() {
			t.Logf("function returned result: %+v", result)
		}
	case reflect.Array, reflect.String:
		if v.Len() != 0 {
			t.Logf("function returned result: %+v", result)
		}
	case reflect.Chan, reflect.Map, reflect.Slice:
		if !v.IsNil() || v.Len() != 0 {
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
