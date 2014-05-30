(c++-mode)

(when (= 0 (buffer-size))

  (mapcar (lambda (i) (insert (concat "#include " "<" i ">\n")))
	  '("algorithm"
	    "atomic"
	    "array"
	    "queue"
	    "bitset"
	    "chrono"
	    "condition_variable"
	    "cstdint"
	    "deque"
	    "forward_list"
	    "fstream"
	    "functional"
	    "iostream"
	    "iomanip"
	    "iterator"
	    "list"
	    "map"
	    "mutex"
	    "numeric"
	    "random"
	    "regex"
	    "ratio"
	    "set"
	    "sstream"
	    "thread"
	    "tuple"
	    "type_traits"
	    "unordered_map"
	    "unordered_set"
	    "vector"
	    "cassert"
	    "cctype"
	    "cstdio"
	    "cstdlib"
	    "cstring"
	    "ctime"))
  (insert "using namespace std;\n")
  (insert "\n")
  (insert "main") (yas-expand)
  (save-buffer))

;; (setq compilation-ask-about-save nil)

(setq scratch-compile-and-run-cmd "g++-4.8 -std=c++11 -O2 -Wall -pedantic -pthread -o .scratch{-,.}cpp && ./.scratch-cpp")
;; (compile scratch-compile-and-run-cmd)
(defun scratch-buffer-p ()
  (string-match-p "^\\.scratch" (file-name-base (buffer-file-name))))

(defun scratch-run ()
  (interactive)
  (shell-command
   (replace-regexp-in-string "\\.\\([^.]*\\)$" "-\\1" (buffer-file-name))))

(compile scratch-compile-and-run-cmd)
(defun scratch-compile-and-run ()
  (interactive)
  (when (scratch-buffer-p)
    (save-buffer)
(message "C+R")
    (let ((current-prefix-arg '(4)))
      ;; (call-interactively 'recompile)
      (recompile)
      ;; (compile scratch-compile-and-run-cmd)
    ;; (scratch-run)
)))

(local-set-key (kbd "C-c r") 'scratch-run)
(local-set-key (kbd "C-c c") 'scratch-compile-and-run)
