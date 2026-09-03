package contract

// StdTable is what generated_test.go reaches for from outside the package. It
// is an external test because it imports the packages whose types it renders,
// and those import this one — an in-package test importing them back is a
// cycle Go refuses outright.
var StdTable = std
