;;; helm-rdio.el -- Control Rdio from Helm

(require 'helm)
(require 'json)
(require 'request)

(defconst rdio-base "http://api.rdio.com/1/"
  "Base URI")

(defconst echonest-key "M7PZ9IZO8UT86Q8E5")
(defconst echonest-base "http://developer.echonest.com/api/v4")

;; (request (concat echonest-base "song/search")
;;          :params `(("combined" . "opeth")
;;                    ("api_key" . ,echonest-key)
;;                    ("results" . "50"))
;;                                         ;         :parser 'json-read
;; ;         :sync t
;;          :parser 'json-read
;;          :success (function*
;;                    (lambda (&key data &allow-other-keys)
;;                      (let* ((response (assoc-default 'response data))
;;                             (songs (assoc-default 'songs response)))
;;                        (message "%s" songs))))
;;          )

(defun alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (alist-get (cdr symbols)
                 (assoc (car symbols) alist))
    (cdr alist)))

(defun rdio-format (track)
  (let ((text (format "%s - %s" (alist-get '(title) track)
                                (alist-get '(artist_name) track))))
    (cons text track)))

(defun rdio-search-sync (search-term)
  "Search spotify for SEARCH-TERM, returning the results as a Lisp structure."
  (let* ((a-url (format "%s/song/search?combined=%s&api_key=%s&bucket=%s&bucket=%s&results=%d"
                        echonest-base
                        search-term
                        echonest-key
                        "id:rdio-US"
                        "tracks"
                        50))
         (response
          (with-current-buffer
              (url-retrieve-synchronously a-url)
            (goto-char url-http-end-of-headers)
            (json-read))))
    (mapcar #'rdio-format
            (rdio-filter-valid
             (alist-get '(response songs) response)))))

(defun rdio-filter-valid (tracks)
  (remove-if
   (lambda (track)
     (zerop (length (alist-get '(tracks) track))))
   tracks))

;; (defun rdio-search (query)
;;   (request (concat echonest-base "song/search")
;;            :params `(("combined" . ,query)
;;                      ("api_key"  . ,echonest-key)
;;                      ("results"  . "2")
;;                      ("bucket"   . "id:rdio-US")
;;                      ("bucket"   . "tracks"))
;;            :parser 'json-read
;;            :sync t
;;            :success (function*
;;                      (lambda (&key data &allow-other-keys)
;;                        (let ((tracks (alist-get '(response songs) data)))
;;                          (setq helm-test-cache
;;                                (mapcar #'rdio-format tracks))
;;                          (and helm-alive-p (helm-update)))))))

(defun helm-rdio-search ()
  (rdio-search-sync helm-pattern))

(defun rdio-unregion (href)
  "Strip off the \"rdio-US:track:\" prefix that EchoNest returns."
  (car (last (split-string href ":"))))

(defun rdio-play-track (track)
  (rdio-play-href
   (rdio-unregion
    (alist-get '(foreign_id)
               (elt (alist-get '(tracks) track) 0)))))

(defun rdio-play-album (track)
  (rdio-play-href
   (rdio-unregion
    (alist-get '(foreign_release_id)
               (elt (alist-get '(tracks) track) 0)))))

(defun rdio-play-href (href)
  (shell-command
   (format "osascript -e 'tell application %S to play source %S'"
           "Rdio"
           href)))

(defun helm-rdio-actions-for-track (actions track)
  "Return a list of helm ACTIONS available for this TRACK."
  `((,(format "Play Track - %s" (alist-get '(title) track)) . rdio-play-track)
    (,(format "Play Album of - %s" (alist-get '(title) track)) . rdio-play-album)
    ("Show Track Metadata" . pp)))

;;;###autoload
(defvar helm-source-rdio-track-search
  '((name . "Rdio")
    (volatile)
    (delayed)
    (multiline)
    (requires-pattern . 2)
    (candidates-process . helm-rdio-search)
    (action-transformer . helm-rdio-actions-for-track)))

;;;###autoload
(defun helm-rdio ()
  "Bring up a Rdio search interface in helm."
  (interactive)
  (helm :sources '(helm-source-rdio-track-search)
        :buffer "*helm-rdio*"))

(provide 'helm-rdio)
