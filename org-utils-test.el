;;; org-utils-test.el --- Tests for org-utils.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Diacus Magnuz

;; Author: Diacus Magnuz <diacus.magnuz@gmail.com>
;; URL: https://github.com/diacus/org-utils

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for org-utils.el using ERT.

;;; Code:

(require 'ert)
(require 'org-utils)


;;;; Timestamp formatting tests

(ert-deftest org-utils-test-format-active-timestamp ()
  "Test `org-utils/format-active-timestamp' output format."
  (let ((time (encode-time 0 0 0 15 1 2025)))
    (should (string= (org-utils/format-active-timestamp time)
                    "<2025-01-15 Wed>"))))

(ert-deftest org-utils-test-format-active-timestamp-leap-year ()
  "Test active timestamp on leap year date."
  (let ((time (encode-time 0 0 0 29 2 2024)))
    (should (string= (org-utils/format-active-timestamp time)
                    "<2024-02-29 Thu>"))))

(ert-deftest org-utils-test-format-active-timestamp-with-time ()
  "Test `org-utils/format-active-timestamp' includes time when present."
  (let ((time (encode-time 0 30 10 15 1 2025)))
    (should (string= (org-utils/format-active-timestamp time)
                    "<2025-01-15 Wed 10:30>"))))

(ert-deftest org-utils-test-format-active-timestamp-midnight ()
  "Test active timestamp at midnight omits time."
  (let ((time (encode-time 0 0 0 15 1 2025)))
    (should (string= (org-utils/format-active-timestamp time)
                    "<2025-01-15 Wed>"))))

(ert-deftest org-utils-test-format-inactive-timestamp ()
  "Test `org-utils/format-inactive-timestamp' output format."
  (let ((time (encode-time 0 0 0 15 1 2025)))
    (should (string= (org-utils/format-inactive-timestamp time)
                    "[2025-01-15 Wed]"))))

(ert-deftest org-utils-test-format-inactive-timestamp-year-end ()
  "Test inactive timestamp on year boundary."
  (let ((time (encode-time 0 0 0 31 12 2025)))
    (should (string= (org-utils/format-inactive-timestamp time)
                    "[2025-12-31 Wed]"))))

(ert-deftest org-utils-test-format-inactive-timestamp-with-time ()
  "Test `org-utils/format-inactive-timestamp' includes time when present."
  (let ((time (encode-time 0 45 14 15 1 2025)))
    (should (string= (org-utils/format-inactive-timestamp time)
                    "[2025-01-15 Wed 14:45]"))))


;;;; Date arithmetic tests

(ert-deftest org-utils-test-add-months-same-year ()
  "Test `org-utils/add-months' within same year."
  (let* ((time (encode-time 0 0 0 15 3 2025))
         (result (org-utils/add-months time 2))
         (decoded (decode-time result)))
    (should (= (decoded-time-month decoded) 5))
    (should (= (decoded-time-year decoded) 2025))
    (should (= (decoded-time-day decoded) 15))))

(ert-deftest org-utils-test-add-months-year-rollover ()
  "Test `org-utils/add-months' crossing year boundary."
  (let* ((time (encode-time 0 0 0 15 11 2025))
         (result (org-utils/add-months time 3))
         (decoded (decode-time result)))
    (should (= (decoded-time-month decoded) 2))
    (should (= (decoded-time-year decoded) 2026))
    (should (= (decoded-time-day decoded) 15))))

(ert-deftest org-utils-test-add-months-day-adjustment ()
  "Test `org-utils/add-months' adjusts day for shorter months."
  (let* ((time (encode-time 0 0 0 31 1 2025))
         (result (org-utils/add-months time 1))
         (decoded (decode-time result)))
    (should (= (decoded-time-month decoded) 2))
    (should (= (decoded-time-day decoded) 28))))

(ert-deftest org-utils-test-add-months-leap-year ()
  "Test `org-utils/add-months' handles leap year February."
  (let* ((time (encode-time 0 0 0 31 1 2024))
         (result (org-utils/add-months time 1))
         (decoded (decode-time result)))
    (should (= (decoded-time-month decoded) 2))
    (should (= (decoded-time-day decoded) 29))))

(ert-deftest org-utils-test-add-months-zero ()
  "Test `org-utils/add-months' with zero months."
  (let* ((time (encode-time 0 0 0 15 6 2025))
         (result (org-utils/add-months time 0))
         (decoded (decode-time result)))
    (should (= (decoded-time-month decoded) 6))
    (should (= (decoded-time-year decoded) 2025))))


;;;; Timedelta tests

(ert-deftest org-utils-test-make-timedelta-seconds ()
  "Test `org-utils/make-timedelta' with seconds only."
  (should (= (org-utils/make-timedelta :seconds 30) 30)))

(ert-deftest org-utils-test-make-timedelta-minutes ()
  "Test `org-utils/make-timedelta' with minutes."
  (should (= (org-utils/make-timedelta :minutes 5) 300)))

(ert-deftest org-utils-test-make-timedelta-hours ()
  "Test `org-utils/make-timedelta' with hours."
  (should (= (org-utils/make-timedelta :hours 2) 7200)))

(ert-deftest org-utils-test-make-timedelta-days ()
  "Test `org-utils/make-timedelta' with days."
  (should (= (org-utils/make-timedelta :days 3) 259200)))

(ert-deftest org-utils-test-make-timedelta-combined ()
  "Test `org-utils/make-timedelta' with multiple units."
  (should (= (org-utils/make-timedelta :days 1 :hours 2 :minutes 30 :seconds 15)
             95415)))

(ert-deftest org-utils-test-make-timedelta-empty ()
  "Test `org-utils/make-timedelta' with no arguments."
  (should (= (org-utils/make-timedelta) 0)))

(ert-deftest org-utils-test-add-timedelta-days ()
  "Test `org-utils/add-timedelta' adding days."
  (let* ((time (encode-time 0 0 0 10 1 2025))
         (result (org-utils/add-timedelta time :days 7))
         (decoded (decode-time result)))
    (should (= (decoded-time-day decoded) 17))
    (should (= (decoded-time-month decoded) 1))
    (should (= (decoded-time-year decoded) 2025))))

(ert-deftest org-utils-test-add-timedelta-month-boundary ()
  "Test `org-utils/add-timedelta' crossing month boundary."
  (let* ((time (encode-time 0 0 0 28 2 2025))
         (result (org-utils/add-timedelta time :days 3))
         (decoded (decode-time result)))
    (should (= (decoded-time-day decoded) 3))
    (should (= (decoded-time-month decoded) 3))
    (should (= (decoded-time-year decoded) 2025))))

(ert-deftest org-utils-test-add-timedelta-hours ()
  "Test `org-utils/add-timedelta' adding hours."
  (let* ((time (encode-time 0 30 10 15 1 2025))
         (result (org-utils/add-timedelta time :hours 5))
         (decoded (decode-time result)))
    (should (= (decoded-time-hour decoded) 15))
    (should (= (decoded-time-minute decoded) 30))))

(ert-deftest org-utils-test-add-timedelta-combined ()
  "Test `org-utils/add-timedelta' with multiple units."
  (let* ((time (encode-time 0 0 12 15 1 2025))
         (result (org-utils/add-timedelta time :days 1 :hours 12))
         (decoded (decode-time result)))
    (should (= (decoded-time-day decoded) 17))
    (should (= (decoded-time-hour decoded) 0))))


(ert-deftest org-utils-test-add-timedelta-months ()
  "Test `org-utils/add-timedelta' adding months."
  (let* ((time (encode-time 0 0 0 15 3 2025))
         (result (org-utils/add-timedelta time :months 2))
         (decoded (decode-time result)))
    (should (= (decoded-time-month decoded) 5))
    (should (= (decoded-time-day decoded) 15))))

(ert-deftest org-utils-test-add-timedelta-months-day-clamp ()
  "Test `org-utils/add-timedelta' clamps day when adding months."
  (let* ((time (encode-time 0 0 0 31 1 2025))
         (result (org-utils/add-timedelta time :months 1))
         (decoded (decode-time result)))
    (should (= (decoded-time-month decoded) 2))
    (should (= (decoded-time-day decoded) 28))))

(ert-deftest org-utils-test-add-timedelta-months-and-days ()
  "Test `org-utils/add-timedelta' with months and days combined."
  (let* ((time (encode-time 0 0 0 15 1 2025))
         (result (org-utils/add-timedelta time :months 1 :days 5))
         (decoded (decode-time result)))
    (should (= (decoded-time-month decoded) 2))
    (should (= (decoded-time-day decoded) 20))))

(ert-deftest org-utils-test-add-timedelta-months-year-rollover ()
  "Test `org-utils/add-timedelta' with months crossing year boundary."
  (let* ((time (encode-time 0 0 0 15 11 2025))
         (result (org-utils/add-timedelta time :months 3))
         (decoded (decode-time result)))
    (should (= (decoded-time-month decoded) 2))
    (should (= (decoded-time-year decoded) 2026))))


;;;; Markdown to Org conversion tests

(ert-deftest org-utils-test-convert-headers ()
  "Test conversion of Markdown headers to Org."
  (with-temp-buffer
    (insert "# Heading 1\n## Heading 2\n### Heading 3\n")
    (org-utils/convert-markdown-buffer-to-org)
    (should (string-match-p "^\\* Heading 1$" (buffer-string)))
    (should (string-match-p "^\\*\\* Heading 2$" (buffer-string)))
    (should (string-match-p "^\\*\\*\\* Heading 3$" (buffer-string)))))

(ert-deftest org-utils-test-convert-bold-double-asterisk ()
  "Test conversion of **bold** text."
  (with-temp-buffer
    (insert "This is **bold** text.")
    (org-utils/convert-markdown-buffer-to-org)
    (should (string-match-p "This is \\*bold\\* text" (buffer-string)))))

(ert-deftest org-utils-test-convert-bold-underscore ()
  "Test conversion of __bold__ text."
  (with-temp-buffer
    (insert "This is __bold__ text.")
    (org-utils/convert-markdown-buffer-to-org)
    (should (string-match-p "This is \\*bold\\* text" (buffer-string)))))

(ert-deftest org-utils-test-convert-italic-asterisk ()
  "Test conversion of *italic* text with asterisk."
  (with-temp-buffer
    (insert "This is *italic* text.")
    (org-utils/convert-markdown-buffer-to-org)
    (should (string-match-p "This is /italic/ text" (buffer-string)))))

(ert-deftest org-utils-test-convert-italic-underscore ()
  "Test conversion of _italic_ text with underscore."
  (with-temp-buffer
    (insert "This is _italic_ text.")
    (org-utils/convert-markdown-buffer-to-org)
    (should (string-match-p "This is /italic/ text" (buffer-string)))))

(ert-deftest org-utils-test-convert-italic-word-boundary ()
  "Test that _word_ inside other text is NOT converted."
  (with-temp-buffer
    (insert "This is my_variable_name here.")
    (org-utils/convert-markdown-buffer-to-org)
    (should (string-match-p "my_variable_name" (buffer-string)))))

(ert-deftest org-utils-test-convert-inline-code ()
  "Test conversion of `inline code`."
  (with-temp-buffer
    (insert "Use `function_name()` for this.")
    (org-utils/convert-markdown-buffer-to-org)
    (should (string-match-p "Use ~function_name()~ for this" (buffer-string)))))

(ert-deftest org-utils-test-convert-fenced-code-block-with-lang ()
  "Test conversion of fenced code block with language."
  (with-temp-buffer
    (insert "```python\ndef hello():\n    pass\n```")
    (org-utils/convert-markdown-buffer-to-org)
    (should (string-match-p "#\\+begin_src python" (buffer-string)))
    (should (string-match-p "#\\+end_src" (buffer-string)))))

(ert-deftest org-utils-test-convert-fenced-code-block-no-lang ()
  "Test conversion of fenced code block without language."
  (with-temp-buffer
    (insert "```\nsome code\n```")
    (org-utils/convert-markdown-buffer-to-org)
    (should (string-match-p "^#\\+begin_src$" (buffer-string)))
    (should (string-match-p "^#\\+end_src$" (buffer-string)))))

(ert-deftest org-utils-test-convert-link-simple ()
  "Test conversion of [text](url) to [[url][text]]."
  (with-temp-buffer
    (insert "See [example](https://example.com) here.")
    (org-utils/convert-markdown-buffer-to-org)
    (should (string-match-p "\\[\\[https://example.com\\]\\[example\\]\\]" (buffer-string)))))

(ert-deftest org-utils-test-convert-link-with-title ()
  "Test that link titles are stripped during conversion."
  (with-temp-buffer
    (insert "[text](https://example.com \"title\")")
    (org-utils/convert-markdown-buffer-to-org)
    (should (string-match-p "\\[\\[https://example.com\\]\\[text\\]\\]" (buffer-string)))))

(ert-deftest org-utils-test-convert-unordered-list-dash ()
  "Test conversion of - unordered list items."
  (with-temp-buffer
    (insert "- Item one\n- Item two")
    (org-utils/convert-markdown-buffer-to-org)
    (should (string-match-p "^- Item one$" (buffer-string)))
    (should (string-match-p "^- Item two$" (buffer-string)))))

(ert-deftest org-utils-test-convert-unordered-list-asterisk ()
  "Test conversion of * unordered list items."
  (with-temp-buffer
    (insert "* Item one\n* Item two")
    (org-utils/convert-markdown-buffer-to-org)
    (should (string-match-p "^- Item one$" (buffer-string)))
    (should (string-match-p "^- Item two$" (buffer-string)))))

(ert-deftest org-utils-test-convert-ordered-list ()
  "Test that ordered lists maintain proper spacing."
  (with-temp-buffer
    (insert "1. First\n2.Second")  ;; second has no space
    (org-utils/convert-markdown-buffer-to-org)
    (should (string-match-p "^1\\. First$" (buffer-string)))
    ;; Note: items without space after '.' aren't recognized as lists
    (should (string-match-p "2\\. Second" (buffer-string)))))

(ert-deftest org-utils-test-convert-complex-document ()
  "Test conversion of a complex multi-element document."
  (with-temp-buffer
    (insert "# Title\n\nSome **bold** and _italic_ text.\n\n")
    (insert "```elisp\n(message \"hello\")\n```\n\n")
    (insert "- List item 1\n- List item 2\n\n")
    (insert "See [link](https://example.com).\n")
    (org-utils/convert-markdown-buffer-to-org)
    (let ((result (buffer-string)))
      (should (string-match-p "^\\* Title$" result))
      (should (string-match-p "\\*bold\\*" result))
      (should (string-match-p "/italic/" result))
      (should (string-match-p "#\\+begin_src elisp" result))
      (should (string-match-p "#\\+end_src" result))
      (should (string-match-p "^- List item" result))
      (should (string-match-p "\\[\\[https://example.com\\]\\[link\\]\\]" result)))))

(ert-deftest org-utils-test-convert-sets-org-mode ()
  "Test that conversion sets the buffer to org-mode."
  (with-temp-buffer
    (insert "# Title\n")
    (text-mode)  ; Start with a different mode
    (org-utils/convert-markdown-buffer-to-org)
    (should (eq major-mode 'org-mode))))


(provide 'org-utils-test)

;;; org-utils-test.el ends here