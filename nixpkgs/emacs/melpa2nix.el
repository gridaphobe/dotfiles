(require 'package-build)

(package-initialize)

(defun melpa2nix-install-package (pkg-info)
  (package-install-file (pb/archive-file-name pkg-info)))

(defun melpa2nix-build-package ()
  (defvar command-line-args-left)	;Avoid 'free variable' warning
  (if (not noninteractive)
      (error "`melpa2nix-build-package' is to be used only with -batch"))
  (pcase command-line-args-left
    (`(,elpa ,package ,version . ,files)
     (progn (setq package-user-dir elpa)
            (melpa2nix-install-package 
             (melpa2nix-package-build-archive package version files))))))

(setq package-build-working-dir (expand-file-name ".")
      package-build-archive-dir (expand-file-name "."))

(defun melpa2nix-package-build-archive (name version files)
  "Build a package archive for package NAME."
  (let* ((pkg-working-dir
          (file-name-as-directory
           (expand-file-name name package-build-working-dir))))


    (pb/message "\n;;; %s\n" name)
    (let* (;(default-directory package-build-working-dir)
           (start-time (current-time))
           (archive-entry (package-build-package name
                                                 version
                                                 files
                                                 "."
                                                 package-build-archive-dir)))
      (princ archive-entry)

      ;; (pb/dump archive-entry (pb/entry-file-name archive-entry))

      (pb/message "Built in %.3fs, finished at %s"
                  (time-to-seconds (time-since start-time))
                  (current-time-string))

      archive-entry)))
