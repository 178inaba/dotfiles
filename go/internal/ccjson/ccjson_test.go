package ccjson

import (
	"encoding/json"
	"testing"
)

func TestDecode(t *testing.T) {
	tests := []struct {
		name  string
		stdin string
		ok    bool
	}{
		{name: "an object", stdin: `{"a":1}`, ok: true},
		// A null document is valid input that every lookup then resolves to
		// nothing, which is not the same as no input at all.
		{name: "null", stdin: `null`, ok: true},
		{name: "no input", stdin: ""},
		// $(cat) drops trailing newlines, so input that is nothing but
		// newlines is no input.
		{name: "only newlines", stdin: "\n\n\n"},
		{name: "malformed", stdin: `{"a":`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if _, ok := Decode([]byte(tt.stdin)); ok != tt.ok {
				t.Errorf("Decode(%q) ok = %t, want %t", tt.stdin, ok, tt.ok)
			}
		})
	}
}

func TestLookup(t *testing.T) {
	tests := []struct {
		name    string
		doc     string
		path    []string
		want    string
		wantErr bool
	}{
		{name: "a value", doc: `{"a":{"b":"x"}}`, path: []string{"a", "b"}, want: "x"},
		// A missing branch is null and indexes to null, so an unrelated field
		// being absent costs nothing.
		{name: "a missing branch", doc: `{}`, path: []string{"a", "b"}},
		{name: "an explicit null branch", doc: `{"a":null}`, path: []string{"a", "b"}},
		// Anything else refuses to be indexed, and jq abandons the whole
		// document rather than just this path.
		{name: "a scalar branch", doc: `{"a":"x"}`, path: []string{"a", "b"}, wantErr: true},
		{name: "an array branch", doc: `{"a":[]}`, path: []string{"a", "b"}, wantErr: true},
		{name: "a scalar document", doc: `"x"`, path: []string{"a"}, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			doc, ok := Decode([]byte(tt.doc))
			if !ok {
				t.Fatalf("Decode(%q) failed", tt.doc)
			}
			got, err := Lookup(doc, tt.path)
			if (err != nil) != tt.wantErr {
				t.Fatalf("Lookup error = %v, want error = %t", err, tt.wantErr)
			}
			if err == nil && ToString(got) != tt.want {
				t.Errorf("Lookup = %q, want %q", ToString(got), tt.want)
			}
		})
	}
}

func TestToString(t *testing.T) {
	tests := []struct {
		name string
		in   any
		want string
	}{
		// // in jq replaces only null and false, so a zero is a value and
		// really does reach the display.
		{name: "null", in: nil},
		{name: "false", in: false},
		{name: "true", in: true, want: "true"},
		{name: "zero", in: json.Number("0"), want: "0"},
		// tostring hands back the literal the input carried, trailing zero and
		// all, which a float64 round trip would lose.
		{name: "a trailing zero", in: json.Number("1.230"), want: "1.230"},
		{name: "a large integer", in: json.Number("9999999999999999999"), want: "9999999999999999999"},
		{name: "a string", in: "x", want: "x"},
		{name: "an empty string", in: ""},
		{name: "an object", in: map[string]any{"a": json.Number("1")}, want: `{"a":1}`},
		{name: "an array", in: []any{json.Number("1")}, want: "[1]"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := ToString(tt.in); got != tt.want {
				t.Errorf("ToString(%v) = %q, want %q", tt.in, got, tt.want)
			}
		})
	}
}
