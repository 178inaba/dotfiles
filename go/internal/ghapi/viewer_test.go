package ghapi_test

import (
	"fmt"
	"net/http"
	"testing"

	"github.com/178inaba/dotfiles/go/internal/ghapi"
	"github.com/178inaba/dotfiles/go/internal/ghapi/ghapitest"
)

func TestViewer(t *testing.T) {
	t.Parallel()

	var gotPath string
	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotPath = r.URL.Path
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprint(w, `{"login":"178inaba","id":12012186}`)
	}))

	got, err := c.Viewer(t.Context())
	if err != nil {
		t.Fatalf("Viewer: %v", err)
	}
	if want := "178inaba"; got != want {
		t.Errorf("Viewer = %q, want %q", got, want)
	}
	if want := "/user"; gotPath != want {
		t.Errorf("Viewer requested %q, want %q", gotPath, want)
	}
}

func TestViewerReportsAFailure(t *testing.T) {
	t.Parallel()

	c := ghapitest.New(t, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(http.StatusUnauthorized)
		fmt.Fprint(w, `{"message":"Bad credentials"}`)
	}))

	if _, err := c.Viewer(t.Context()); err == nil {
		t.Error("Viewer succeeded on bad credentials, want a failure")
	} else if status, ok := ghapi.HTTPStatus(err); !ok || status != http.StatusUnauthorized {
		t.Errorf("Viewer error carried status %d (%t), want 401", status, ok)
	}
}
