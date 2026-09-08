package frontmatter_test

import (
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"

	"github.com/178inaba/dotfiles/go/internal/frontmatter"
)

func TestSplit(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		content string
		want    frontmatter.Block
		wantOK  bool
	}{
		{
			name:    "a block with LF",
			content: "---\nname: a\ndescription: b\n---\n\nbody\n",
			want:    frontmatter.Block{Lines: []string{"name: a", "description: b"}, Start: 2},
			wantOK:  true,
		},
		{
			// The drift this package was made to end: the same block saved
			// with CRLF has to read as the same block.
			name:    "the same block with CRLF",
			content: "---\r\nname: a\r\ndescription: b\r\n---\r\n\r\nbody\r\n",
			want:    frontmatter.Block{Lines: []string{"name: a", "description: b"}, Start: 2},
			wantOK:  true,
		},
		{
			name:    "no opening fence",
			content: "name: a\n---\n",
		},
		{
			// The body would parse as YAML often enough to be mistaken for
			// frontmatter, which is why the extent is read from the fences.
			name:    "no closing fence",
			content: "---\nname: a\n\nbody\n",
		},
		{
			name:    "an empty block",
			content: "---\n---\n",
			want:    frontmatter.Block{Start: 2},
			wantOK:  true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			got, ok := frontmatter.Split([]byte(tt.content))
			if ok != tt.wantOK {
				t.Fatalf("Split ok = %v, want %v", ok, tt.wantOK)
			}
			if diff := cmp.Diff(tt.want, got, cmpopts.EquateEmpty()); diff != "" {
				t.Errorf("Split block mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

func TestBlockFields(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		content string
		want    map[string]any
		wantErr bool
	}{
		{
			name:    "a mapping",
			content: "---\nname: a\n---\n",
			want:    map[string]any{"name": "a"},
		},
		{
			// Not a parse failure: a block that holds no mapping holds no
			// fields, which is the same as holding none.
			name:    "a sequence",
			content: "---\n- a\n- b\n---\n",
			want:    map[string]any{},
		},
		{
			name:    "a block that does not parse",
			content: "---\nargument-hint: [<a>] [--b]\n---\n",
			wantErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()

			block, ok := frontmatter.Split([]byte(tt.content))
			if !ok {
				t.Fatal("Split found no block")
			}
			got, err := block.Fields()
			if tt.wantErr {
				if err == nil {
					t.Fatal("Fields returned no error")
				}
				return
			}
			if err != nil {
				t.Fatalf("Fields: %v", err)
			}
			if diff := cmp.Diff(tt.want, got, cmpopts.EquateEmpty()); diff != "" {
				t.Errorf("Fields mismatch (-want +got):\n%s", diff)
			}
		})
	}
}
