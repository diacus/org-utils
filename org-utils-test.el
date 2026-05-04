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


(provide 'org-utils-test)

;;; org-utils-test.el ends here