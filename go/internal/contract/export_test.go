package contract

// The two below are what generated_test.go reaches for from outside the
// package. It is an external test because it imports the packages whose types
// it renders, and those import this one — an in-package test importing them
// back is a cycle Go refuses outright.

// StdTable is the table ccx renders against, which the golden test compares
// against a reading of the sources.
var StdTable = std

// StdMarshalers is the exception list std is built with, which the golden test
// needs to build a table from those same sources.
var StdMarshalers = marshalers
