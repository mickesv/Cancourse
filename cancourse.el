;;; cancourse --- View a Canvas course -*- lexical-binding t; -*-
;;
;;; Copyright (c) 2021 Mikael Svahnberg
;;
;;; License:
;; The MIT License (MIT)
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;; I used https://github.com/paulodder/canvas-emacs as a starting point.
;; Canvas REST Api documentation: https://canvas.instructure.com/doc/api/index.html
;;
;; SETUP
;; 0. clone this repo.
;; 1. Generate a canvas token under Account > Setting > Approved Integrations
;; 2. set `canvas-token` to this token.
;; 3. Set `canvas-baseurl` to the base url for your institution
;;    (for example "https://canvas.baseurl.com")
;; 4. M-x cancourse
;; 5. ...
;; 6. Profit!
;;
;; Alternative with use-package
;; First, clone the repository somewhereto (e.g. your site-lisp dir).
;; 
;; (use-package cancourse
;;   :load-path "~/.emacs.d/site-lisp/cancourse"
;;   :config
;;   (setq canvas-baseurl "https://canvas.baseurl.com"
;;         canvas-token   "<your-token>"))
;;

;;; Code:

;; TODO: refactor the insert-modules mess and make it generically usable
;; TODO: insert-assignments and insert-announcements are so similar so they ought to be mergable
;; TODO: Insert "open course in browser" button
;; TODO: Insert "Open discussion in browser" buttons

(require 'request)
(require 'json)

(defgroup cancourse nil "Canvas Course variables")

(defcustom canvas-baseurl nil
  "Base URL of canvas environment."
  :type 'string
  :group 'cancourse)

(defcustom canvas-token nil
  "Canvas token."
  :type 'string
  :group 'cancourse)

(defcustom cancourse-collapsed-marker "|show/hide]"
  "Marker to indicate that something is collapsed."
  :type 'string
  :group 'cancourse)

(defface cancourse-course-title-face
  '((t :inherit 'org-level-1))
  "Face for Course Title"
  :group 'cancourse)

(defface cancourse-section-heading-face
  '((t :inherit 'org-level-2))
  "Face for Cancourse section heading"
  :group 'cancourse)
       
(defface cancourse-collapsed-marker-face
  '((t :inherit 'org-ellipsis))
  "Face for cancourse collapsed item"
  :group 'cancourse)

;; Internal variables
(defvar cancourse--course-list nil)
(defvar cancourse--userid nil)

;; Canvas API endpoints and general http stuff
;; --------------------
(defconst cancourse--api-items-to-retreive 100)
(defconst cancourse--api-userid "/api/v1/users/self")
(defconst cancourse--api-list-courses "/api/v1/users/%s/courses")
(defconst cancourse--api-announcements "/api/v1/announcements")
(defconst cancourse--api-frontpage "/api/v1/courses/%s/front_page")
(defconst cancourse--api-list-modules "/api/v1/courses/%s/modules")
(defconst cancourse--api-list-assignments "/api/v1/courses/%s/assignments")
(defconst cancourse--api-list-discussions "/api/v1/courses/%s/discussion_topics")

(defun cancourse--encode-params (params)
  "Encode PARAMS alist into http get format."
  (concat "?"
          (mapconcat (lambda (keyval)
                       (format "%s=%s" (car keyval) (cdr keyval)))
                     params
                     "&")))

(defun cancourse--get (endpoint &optional treatment request-type request-params)
  "Retrieve url ENDPOINT. TREATMENT is either 'json or 'raw.
REQUEST-TYPE is usually GET.
REQUEST-PARAMS is an alist ((param-name . param value) ... )"
  (let ((json-params (json-encode request-params))
        (target (concat
                 (if (not (string-prefix-p canvas-baseurl endpoint)) canvas-baseurl "")
                 endpoint
                 (if (string= request-type "GET") (cancourse--encode-params request-params)))))
    (message "Cancourse requesting page %s" target)
    (if canvas-token
        (request-response-data (request target
                                 :type (or request-type "GET")
                                 :headers `(("Authorization" . ,(concat "Bearer " canvas-token))
                                            ("Content-Type" . "application/json"))
                                 :sync t
                                 :data json-params
                                 :parser (lambda ()
                                           (cond ((equal treatment 'raw) (buffer-string))
                                                 (t (json-read))))
                                 :success (cl-function
                                           (lambda (&key data &allow-other-keys) data))
                                 :error (cl-function
                                         (lambda (&key error-thrown data status &allow-other-keys)
                                           (message "Cancourse retreive error: %s" error-thrown)
                                           (concat "")))))
      (user-error "Cancourse retreive error: `canvas-token' not set"))))

(defun cancourse--render-page (contents)
  "Render as html CONTENTS and return as a string."
  (with-temp-buffer
    (insert (or contents ""))
    (shr-render-region (point-min) (point-max))
    (buffer-string)))

;; Retrievers

(defun cancourse--get-userid (&optional reload)
  "Get User id. RELOAD if t."
  (if (or reload (not cancourse--userid))
      (setq cancourse--userid
            (cdar (cancourse--get-value '(id)
                                        (cancourse--get cancourse--api-userid))))
    cancourse--userid))

(defun cancourse--get-course-list (&optional reload)
  "Get list of courses for user. RELOAD if t."
  (cancourse--get (format cancourse--api-list-courses
                          (cancourse--get-userid reload))
                  'json
                  "GET"
                  '((per_page . 100))))

(defun cancourse--get-announcements (courseid)
  "Get last 90 days of anouncements for COURSEID."
  (let ((time-period (format-time-string "%Y-%m-%d"
                                         (time-subtract (current-time) (days-to-time 90)))))
         (cancourse--get cancourse--api-announcements
                         'json
                         "GET"
                         `((start_date . ,time-period)
                           (end_date . ,(format-time-string "%Y-%m-%d"))
                           ("context_codes" . ,(format "course_%s" courseid))))))

(defun cancourse--get-frontpage (courseid)
  "Get frontpage for COURSEID."
  (cancourse--get (format cancourse--api-frontpage courseid)))

(defun cancourse--get-modules-list (courseid)
  "Get list of modules for COURSEID."
  (cancourse--get (format cancourse--api-list-modules courseid)
                  'json
                  "GET"
                  '((per_page . 100)
                    (include . items))))

(defun cancourse--get-assignments-list (courseid)
  "Get list of assignments for COURSEID."
  (cancourse--get (format cancourse--api-list-assignments courseid)
                  'json
                  "GET"
                  '((per_page . 100))))

(defun cancourse--get-discussions-list (courseid)
  "Get list of discussions for COURSEID."
  (cancourse--get (format cancourse--api-list-discussions courseid)
                  'json
                  "GET"
                  '((per_page . 100))))


;; JSON parsing
;; --------------------
(defun cancourse--get-value (keys body)
  "(Recursively) Find KEYS in BODY."
  (seq-filter (lambda (item)
                (cond
                 ;;((listp (cdr item)) (cancourse--get-value keys (cdr item)))
                 (t (seq-some (lambda (key) (equal key (car item))) keys))))
              body))

(defun cancourse--get-item (key value body)
  "Find and return first item in BODY where KEY is VALUE."
  (seq-find (lambda (item)
              (when (equal value (cdar (cancourse--get-value key item))) item))
            body))


;; Generic Helpers
;; --------------------
(defun cancourse--select-from-prompt (data &optional prompt key-to-show key-to-return)
  "Offer PROMPT based on KEY-TO-SHOW in DATA to user and return KEY-TO-RETURN from selection."
  (let* ((prompt (or prompt "Cancourse: "))
         (key-to-show (or key-to-show '(id)))
         (key-to-return (or key-to-return '(id)))
         (items (seq-map (lambda (item)
                           (cons (cdar (cancourse--get-value key-to-show item))
                                 (cdar (cancourse--get-value key-to-return item))))
                         data))
         (labels (seq-map 'car items))
         (selection (completing-read prompt labels)))
    (cdr (assoc selection items))))

(defun cancourse--insert-heading (name &optional skip-eol)
  "Insert NAME as a heading into current buffer."
  (insert (propertize (or name "")
                      'face 'cancourse-section-heading-face
                      'heading 't)
          (if skip-eol "" "\n")))

(defun cancourse--insert-text-toggle (short-text long-text &optional hidden padding)
  "Insert SHORT-TEXT that can be clicked to expose LONG-TEXT, maybe initially HIDDEN. use PADDING between short and long text."
  (let* ((start-button (point))
         (end-button (progn
                       (insert (propertize (or short-text "no title") 'link-item t))
                       (point)))
         (pad (insert (or padding " ")))
         (start-text (point))
         (overlay (progn
                    (insert (or long-text "no contents"))
                    (make-overlay start-text (point))))
         (button (make-button start-button end-button
                              'action (apply-partially
                                       (lambda (ovl arg)
                                         (let ((vis (overlay-get ovl 'invisible)))
                                           (overlay-put ovl 'invisible (not vis))))
                                       overlay))))
    (overlay-put overlay 'invisible hidden)))

(defun cancourse--insert-collapsible-section (heading body &optional hidden)
  "Insert a whole collapsed (if HIDDEN is t) section BODY under HEADING."
  (cancourse--insert-heading heading t)
  (insert " ")
  (cancourse--insert-text-toggle
   (propertize cancourse-collapsed-marker 'face 'cancourse-collapsed-marker-face)
   body
   hidden
   "\n"))

(defun cancourse--goto-heading (heading)
  "Jump to HEADING."
  (let (match found)
    (goto-char (point-min))
    (while (and (not found)
                (setq match (text-property-search-forward 'heading t t)))
      (setq found (string= heading (buffer-substring
                                    (prop-match-beginning match)
                                    (prop-match-end match)))))))

(defun cancourse--goto-link-item (direction)
  "Jump to next link item. if DIRECTION is positive jump forward, otherwise backwards."
  (let* ((origin (point))
         (search-fun (if (< 0 direction)
                         #'text-property-search-forward
                       #'text-property-search-backward))
         (match (funcall search-fun 'link-item)))
    (while (and (>= origin (prop-match-beginning match))
                (<= origin (prop-match-end match)))
      (setq match (funcall search-fun 'link-item)))
    (when match (goto-char (prop-match-beginning match)))))

;; Page Contents
;; --------------------
(defun cancourse--maybe-reload-course-list (&optional reload)
  "Reload course list if RELOAD is set."
  (when (or reload (not cancourse--course-list))
    (let* ((data (cancourse--get-course-list reload))
           (sorted (sort data (lambda (a b)
                                (string-greaterp (cdar (cancourse--get-value '(start_at) a))
                                                 (cdar (cancourse--get-value '(start_at) b)))))))
      (setq cancourse--course-list sorted))))
  
(defun cancourse--select-course (&optional reload)
  "Prompt user to select a course. if RELOAD, force a reload. Return courseid."
  (cancourse--maybe-reload-course-list reload)
  (cancourse--select-from-prompt cancourse--course-list
                                 "Select Course:"
                                 '(name)))

(defun cancourse--insert-announcements (courseid)
  "Insert announcements for COURSEID."
  (cancourse--insert-heading "Announcements")
  (let* ((announcements (cancourse--get-announcements courseid)))
    (seq-map (lambda (item)
               (let* ((heading (cdar (cancourse--get-value '(title) item)))
                      (posted-at (cdar (cancourse--get-value '(posted_at) item)))
                      (last-reply (cdar (cancourse--get-value '(last_reply) item)))
                      (body (concat (when posted-at (concat "\nPosted: " posted-at))
                                    (when last-reply (concat "\nLast Reply: "  last-reply))
                                    "\n"
                                    (cancourse--render-page
                                     (or (cdar (cancourse--get-value '(message) item)) ""))
                                    "\n")))
                 (insert " - ")
                 (cancourse--insert-text-toggle heading body t)
                 (insert "\n")))
             announcements)))
  
(defun cancourse--insert-frontpage (courseid)
  "Insert collapsed frontpage for COURSEID."
  (let* ((raw (cancourse--get-frontpage courseid))
         (fp (if raw (cancourse--render-page (cdar (cancourse--get-value '(body message) raw))) "no contents")))
    (cancourse--insert-collapsible-section "Frontpage" fp t)))

(defun cancourse--maybe-load-insert-item (overlay)
  "Lookup url property in OVERLAY and insert."
  (when (and (not (overlay-get overlay 'loaded))
             (overlay-get overlay 'url))
    (let* (buffer-read-only
           (page (cancourse--render-page
                  (cdar (cancourse--get-value
                         '(body description)
                         (cancourse--get (overlay-get overlay 'url))))))
           (origin (point))
           (start (progn
                    (goto-char (overlay-end overlay))
                    (forward-char)
                    (point)))
           (end (progn (insert (or page "no contents")) (point)))
           (ov (make-overlay start end)))
      (overlay-put overlay 'body-ov ov)
      (overlay-put overlay 'loaded t)
      (goto-char origin))))
  
(defun cancourse--insert-module-item-contents (ovl arg)
  "Insert the contents of one item in a module."
  (cancourse--maybe-load-insert-item ovl)
  (let ((invis (not (overlay-get ovl 'invisible)))
        (body (overlay-get ovl 'body-ov)))
    (when body (overlay-put body 'invisible invis))
    (overlay-put ovl 'invisible invis)))
  
(defun cancourse--insert-module-items (module)
  "Insert all items in a MODULE."
  (let ((items (cdar (cancourse--get-value '(items) module))))
    (seq-map (lambda (item)
               (let* ((title (or (cdar (cancourse--get-value '(title) item)) "No Title"))
                     (url (cdar (cancourse--get-value '(url) item)))
                     (leader (insert "   - "))
                     (t-start (point))
                     (t-end (progn (insert title) (point)))
                     (padding (insert " "))
                     (c-start (point))
                     (c-end c-start)
                     (ov (make-overlay c-start c-end)))
                 (overlay-put ov 'loaded nil)
                 (overlay-put ov 'url url)
                 (when url
                   (make-button
                    t-start t-end
                    'action (apply-partially
                             'cancourse--insert-module-item-contents
                             ov)))
                 (overlay-put ov 'invisible t)
                 (insert "\n")))
             items)))

(defun cancourse--insert-module (module)
  "Insert a single MODULE."
  (insert " - ")
  (let* ((title (cdar (cancourse--get-value '(name) module)))
         (t-start (point))
         (t-end (progn (insert title) (point)))
         (padding (insert "\n"))
         (c-start (point))
         (contents (cancourse--insert-module-items module))
         (c-end (point))
         (ov (make-overlay c-start c-end))
         (button (make-button t-start t-end
                              'action (apply-partially
                                       (lambda (ovl arg)
                                         (let ((vis (overlay-get ovl 'invisible)))
                                           (overlay-put ovl 'invisible (not vis))))
                                       ov))))
    (overlay-put ov 'invisible t)))

(defun cancourse--insert-modules-list (courseid)
  "Insert the list of modules for COURSEID."
  (cancourse--insert-heading "Modules")
  (let ((modules (cancourse--get-modules-list courseid)))
    (seq-map 'cancourse--insert-module modules)))

(defun cancourse--insert-assignments-list (courseid)
  "Insert the list of assignments in COURSEID."
  (cancourse--insert-heading "Assignments")
  (let ((assignments (cancourse--get-assignments-list courseid)))
    (seq-map (lambda (item)
               (let ((heading (or (cdar (cancourse--get-value '(name) item)) ""))
                     (body (cancourse--render-page (or (cdar (cancourse--get-value '(description) item)) ""))))
                 (insert " - ")
                 (cancourse--insert-text-toggle heading body t "\n")))
             assignments)))

(defun cancourse--insert-discussions-list (courseid)
  "Insert the list of Discussions in COURSEID."
  (cancourse--insert-heading "Discussions")
  (let ((discussions (cancourse--get-discussions-list courseid)))
    (seq-map (lambda (item)
               (let ((heading (cdar (cancourse--get-value '(title) item)))
                     (last-post (cdar (cancourse--get-value '(last_reply_at) item)))
                     (unread (cdar (cancourse--get-value '(unread_count) item)))
                     (url (cdar (cancourse--get-value '(html_url) item)))
                     (body (cancourse--render-page (or (cdar (cancourse--get-value '(message) item)) ""))))
                 (insert " - ")
                 (cancourse--insert-text-toggle (concat (when (< 0 unread) (format "[%d] " unread) "") heading)
                                                body
                                                t
                                                "\n")))
             discussions)))

(defun cancourse--generate-page (courseid)
  "Generate the course page for COURSEID."
  (let* (buffer-read-only
        (course (cancourse--get-item '(id) courseid cancourse--course-list))
        (course-name (cdar (cancourse--get-value '(name) course))))
    (erase-buffer)
    (insert (propertize course-name
                        'face 'cancourse-course-title-face
                        'courseid courseid))
    (insert "\n\n")
    (cancourse--insert-announcements courseid)
    (insert "\n")
    (cancourse--insert-frontpage courseid)
    (insert "\n")
    (cancourse--insert-assignments-list courseid)
    (insert "\n")
    (cancourse--insert-modules-list courseid)
    (insert "\n")
    (cancourse--insert-discussions-list courseid)
    (goto-char (point-min))))

;; Start function
;; --------------------
;;;###autoload
(defun cancourse (&optional reload)
  "View a Canvas Course. C-u cancourse reloads courselist."
  (interactive "P")
  (let* ((courseid (cancourse--select-course reload))
         (course (cancourse--get-item '(id) courseid cancourse--course-list))
         (course-name (cdar (cancourse--get-value '(name) course)))
         (buffer (get-buffer-create (concat "*Canvas Course "
                                            course-name
                                            "*"))))
    (set-buffer buffer)
    (cancourse-mode)
    (cancourse--generate-page courseid)
    (switch-to-buffer buffer)
    (concat "<EOP>")))

;;;###autoload
(defalias 'caco 'cancourse)

;; Keyed commands
;; --------------------
(defun cancourse-view-announcements ()
  "Jump to heading."
  (interactive)
  (cancourse--goto-heading "Announcements"))

(defun cancourse-view-frontpage ()
  "Jump to heading."
  (interactive)
  (cancourse--goto-heading "Frontpage")
  (forward-button 1)
  (push-button))

(defun cancourse-view-assignments ()
  "Jump to heading."
  (interactive)
  (cancourse--goto-heading "Assignments"))

(defun cancourse-view-modules ()
  "Jump to heading."
  (interactive)
  (cancourse--goto-heading "Modules"))

(defun cancourse-view-discussions ()
  "Jump to heading."
  (interactive)
  (cancourse--goto-heading "Discussions"))

(defun cancourse-next-button ()
  "Jump to start of next clickable item."
  (interactive)
  (forward-button 1))

(defun cancourse-prev-button ()
  "Jump to start of previous clickable item."
  (interactive)
  (backward-button 1))

(defun cancourse-next-link-item ()
  "Jump to start of next link item."
  (interactive)
  (cancourse--goto-link-item 1))

(defun cancourse-prev-link-item ()
  "Jump to start of previous link item."
  (interactive)
  (cancourse--goto-link-item -1))

(defun cancourse-reload-page ()
  "Reload and refetch page."
  (interactive)
  (let ((courseid (or (get-text-property (point-min) 'courseid)
                      (cancourse--select-course t))))
    (if courseid
        (cancourse--generate-page courseid)
      (user-error "Cannot find courseid"))))

;; A tiny major mode
;; --------------------

;;;###autoload
(define-derived-mode cancourse-mode special-mode "cancourse"
  "Major mode for viewing Canvas Courses."
  :group 'cancourse)

(define-key cancourse-mode-map (kbd "a") 'cancourse-view-announcements)
(define-key cancourse-mode-map (kbd "s") 'cancourse-view-assignments)
(define-key cancourse-mode-map (kbd "f") 'cancourse-view-frontpage)
(define-key cancourse-mode-map (kbd "m") 'cancourse-view-modules)
(define-key cancourse-mode-map (kbd "d") 'cancourse-view-discussions)
(define-key cancourse-mode-map (kbd "g") 'cancourse-reload-page)
(define-key cancourse-mode-map (kbd "n") 'cancourse-next-link-item)
(define-key cancourse-mode-map (kbd "p") 'cancourse-prev-link-item)
(define-key cancourse-mode-map (kbd "<tab>") 'cancourse-next-button)
(define-key cancourse-mode-map (kbd "<backtab>") 'cancourse-prev-button)

;;; provides:
(provide 'cancourse)

;;; cancourse ends here
