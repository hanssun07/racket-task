#lang racket/base

(require
    gregor
    threading
    "../utils.rkt"
    "ann.rkt")
(provide (all-from-out gregor)
    moment/datum? moment/datum<?
    unwrap-moment wrap-moment
    moment->datum datum->moment
    now)

(:structdef moment/datum : Moment/Datum
    ([moment    : (? Moment)]
     [iso8601   : (? String)]))
(struct moment/datum
    (moment iso8601)
    #:mutable)

(: unwrap-moment (Moment/Datum (-> f) -> (U Moment f))
                 (Moment/Datum f      -> (U Moment f))
                 (Moment/Datum        -> (U Moment (^ Exn:Fail))))
(define (unwrap-moment m
         [on-fail (error-failthrough 'unwrap-moment "~s is not a valid iso8601 datetime"
                                     (moment/datum-iso8601 m))])
    (unless (moment/datum-moment m)
        (with-handlers
            ([exn:fail:contract? (lambda (e) (if (procedure? on-fail) (on-fail) on-fail))])
            (~> (moment/datum-iso8601 m)
                (iso8601->moment _)
                (adjust-timezone _ (current-timezone))
                (set-moment/datum-moment! m _))))
    (moment/datum-moment m))

(: wrap-moment (Moment -> Moment/Datum))
(define (wrap-moment m)
    (moment/datum m #f))
            
(: moment/datum<? (Moment/Datum Moment/Datum -> Bool))
(define (moment/datum<? md1 md2)
    (string<? (moment->datum md1) (moment->datum md2)))

(: moment->datum (Moment/Datum -> String))
(define (moment->datum m)
    (unless (moment/datum-iso8601 m)
        (~> (moment/datum-moment m)
            (adjust-timezone _ 0)
            (moment->iso8601 _)
            (set-moment/datum-iso8601! m _)))
    (moment/datum-iso8601 m))

(: datum->moment (Any (e -> f) -> (U Moment f))
                 (Any f        -> (U Moment f))
                 (Any          -> (U Moment (^ Exn:Fail))))
(define (datum->moment d [on-fail #f])
    (if (string? d)
        (moment/datum #f d)
        (if (procedure? on-fail) (on-fail) on-fail)))

(: now (-> Datum/Moment))
(define (now) (wrap-moment (now/moment)))
