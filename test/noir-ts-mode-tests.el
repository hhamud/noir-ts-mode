;;; noir-ts-mode-tests.el --- Tests for noir-ts-mode  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for noir-ts-mode.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'treesit)
(require 'noir-ts-mode)

(defconst noir-ts-mode-test--repo-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the noir-ts-mode repository root.")

(defconst noir-ts-mode-test--local-grammar-root
  (expand-file-name "../tree-sitter-noir" noir-ts-mode-test--repo-root)
  "Local sibling checkout of tree-sitter-noir, when available.")

(defconst noir-ts-mode-test--remote-grammar-root
  "https://github.com/hhamud/tree-sitter-noir"
  "Fallback remote tree-sitter-noir repository for tests.")

(defconst noir-ts-mode-test--simple-sample
  "use crate::math::{double, SCALE};

pub global N: u32 = 2;

pub struct Point {
    pub x: Field,
    pub y: Field,
}

pub fn main(points: [Point; N]) -> Field {
    let first = points[0];
    double(first.x + SCALE)
}

mod math {
    pub global SCALE: Field = 3;

    pub fn double(x: Field) -> Field {
        x + x
    }
}
"
  "Fixture covering global declarations, structs, modules, and functions.")

(defconst noir-ts-mode-test--modern-sample
  "#[deprecated(\"use fast_area\")]
pub(crate) fn slow_area<T: Area>(shape: T) -> Field
where
    T: Area
{
    shape.area()
}

trait Area {
    fn area(self) -> Field;
}

trait ShapeOps<T> = Area where T: Area;

unconstrained fn control_flow(mut i: u32) {
    let pi = 1.5;
    while i < 10 {
        i += 1;
    }
}
"
  "Fixture covering newer Noir syntax supported by tree-sitter-noir.")

(defconst noir-ts-mode-test--indent-input
  "pub(crate) fn slow_area<T: Area>(shape: T) -> Field
where
T: Area
{
shape.area()
}

unconstrained fn control_flow(mut i: u32) {
while i < 10 {
i += 1;
}
}
"
  "Unindented sample used for indentation tests.")

(defconst noir-ts-mode-test--indent-expected
  "pub(crate) fn slow_area<T: Area>(shape: T) -> Field
where
    T: Area
{
    shape.area()
}

unconstrained fn control_flow(mut i: u32) {
    while i < 10 {
        i += 1;
    }
}
"
  "Expected indentation for `noir-ts-mode-test--indent-input'.")

(defun noir-ts-mode-test--grammar-source ()
  "Return the grammar source to use for tests."
  (or (getenv "NOIR_TS_GRAMMAR_SOURCE")
      (and (file-directory-p noir-ts-mode-test--local-grammar-root)
           noir-ts-mode-test--local-grammar-root)
      noir-ts-mode-test--remote-grammar-root))

(defun noir-ts-mode-test-ensure-grammar ()
  "Install the Noir tree-sitter grammar for tests if needed."
  (setq treesit-language-source-alist
        `((noir ,(noir-ts-mode-test--grammar-source) "main" "src")))
  (unless (treesit-ready-p 'noir)
    (treesit-install-language-grammar 'noir)))

(defmacro noir-ts-mode-test--with-buffer (contents &rest body)
  "Create a temporary Noir buffer with CONTENTS and run BODY inside it."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,contents)
     (setq-local buffer-file-name
                 (expand-file-name "fixture.nr" temporary-file-directory))
     (goto-char (point-min))
     (noir-ts-mode)
     (font-lock-ensure (point-min) (point-max))
     ,@body))

(defun noir-ts-mode-test--line-at-point ()
  "Return the current line without its trailing newline."
  (string-trim
   (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun noir-ts-mode-test--face-at-string (needle &optional occurrence)
  "Return the `face' property at NEEDLE.
If OCCURRENCE is non-nil, use the Nth occurrence of NEEDLE."
  (let ((index (or occurrence 1)))
    (save-excursion
      (goto-char (point-min))
      (dotimes (_ index)
        (unless (search-forward needle nil t)
          (ert-fail (format "Could not find %S in test buffer" needle))))
      (get-text-property (match-beginning 0) 'face))))

(defun noir-ts-mode-test--has-face-p (needle face &optional occurrence)
  "Return non-nil if NEEDLE has FACE at OCCURRENCE."
  (let ((actual (noir-ts-mode-test--face-at-string needle occurrence)))
    (if (listp actual)
        (memq face actual)
      (eq face actual))))

(ert-deftest noir-ts-mode-loads ()
  (should (featurep 'noir-ts-mode)))

(ert-deftest noir-ts-mode-auto-mode-detects-nr-files ()
  (skip-unless (treesit-ready-p 'noir))
  (with-temp-buffer
    (setq-local buffer-file-name
                (expand-file-name "sample.nr" temporary-file-directory))
    (set-auto-mode)
    (should (eq major-mode 'noir-ts-mode))
    (should (treesit-parser-list))))

(ert-deftest noir-ts-mode-font-locks-modern-noir-syntax ()
  (skip-unless (treesit-ready-p 'noir))
  (noir-ts-mode-test--with-buffer
      (concat noir-ts-mode-test--simple-sample "\n" noir-ts-mode-test--modern-sample)
    (should (noir-ts-mode-test--has-face-p "pub" 'font-lock-keyword-face 1))
    (should (noir-ts-mode-test--has-face-p "global" 'font-lock-keyword-face 1))
    (should (noir-ts-mode-test--has-face-p "trait" 'font-lock-keyword-face 1))
    (should (noir-ts-mode-test--has-face-p "where" 'font-lock-keyword-face 1))
    (should (noir-ts-mode-test--has-face-p "unconstrained" 'font-lock-keyword-face 1))
    (should (noir-ts-mode-test--has-face-p "ShapeOps" 'font-lock-type-face 1))
    (should (noir-ts-mode-test--has-face-p "slow_area" 'font-lock-function-name-face 1))
    (should (noir-ts-mode-test--has-face-p "1.5" 'font-lock-constant-face 1))))

(ert-deftest noir-ts-mode-indents-modern-noir-syntax ()
  (skip-unless (treesit-ready-p 'noir))
  (noir-ts-mode-test--with-buffer noir-ts-mode-test--indent-input
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string) noir-ts-mode-test--indent-expected))))

(ert-deftest noir-ts-mode-beginning-of-defun-handles-new-definition-kinds ()
  (skip-unless (treesit-ready-p 'noir))
  (noir-ts-mode-test--with-buffer noir-ts-mode-test--modern-sample
    (search-forward "shape.area()")
    (beginning-of-defun)
    (should (equal (noir-ts-mode-test--line-at-point)
                   "pub(crate) fn slow_area<T: Area>(shape: T) -> Field"))

    (goto-char (point-min))
    (search-forward "fn area(self) -> Field;")
    (beginning-of-defun)
    (should (equal (noir-ts-mode-test--line-at-point)
                   "fn area(self) -> Field;"))

    (goto-char (point-min))
    (search-forward "let pi = 1.5;")
    (beginning-of-defun)
    (should (equal (noir-ts-mode-test--line-at-point)
                   "unconstrained fn control_flow(mut i: u32) {"))))

(defun noir-ts-mode-test-run ()
  "Install the Noir grammar if needed and run the ERT suite."
  (interactive)
  (noir-ts-mode-test-ensure-grammar)
  (ert-run-tests-batch-and-exit))

(provide 'noir-ts-mode-tests)

;;; noir-ts-mode-tests.el ends here
