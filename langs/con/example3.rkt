#lang racket
(if (if (zero? 0) 0 1) 1 0) ; should eval to 1
