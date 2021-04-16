# Cancourse
Canvas Course Viewer

I used https://github.com/paulodder/canvas-emacs as a starting point.

## Setup

1. Generate a canvas token under Account > Setting > Approved Integrations
2. set `canvas-token` to this token.
3. Set `canvas-baseurl` to the base url for your institution
   (for example "https://canvas.baseurl.com")
4. M-x cancourse
5. ...
6. Profit!

Alternative with use-package

First, clone the repository somewhereto (e.g. your site-lisp dir).

```
(use-package cancourse
  :load-path "~/.emacs.d/site-lisp/cancourse"
  :config
  (setq canvas-baseurl "https://canvas.baseurl.com"
        canvas-token   "<your-token>"))
```

## Commands

Key | command
a | Jump to Announcements
s | Jump to Assignments
f | Jump to (and view) Frontpahe
m | Jump to Modules
d | Jump to Discussions
g | Reload page
n or <tab> | Next clickable item
p or <backtab> | Previous clickable item
