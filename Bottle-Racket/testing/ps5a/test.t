#!/usr/bin/perl
use 5.10.0;
use warnings FATAL => 'all';

use File::Temp;
use File::Copy;
use IO::Handle;

sub scm_eval {
    my ($fname, $cmdline) = @_;
    my $tmpcmd = File::Temp->new;
    $tmpcmd->say($cmdline);
    my $tmpcode = File::Temp->new;
    copy($fname, $tmpcode) or die "Copy failed: $!";
    open(my $fh, '>>', $tmpcode);
    $fh->say('(define-namespace-anchor bottlenose-nsa)
(define bottlenose-ns (namespace-anchor->namespace bottlenose-nsa))
(eval (call-with-input-string (vector-ref (current-command-line-arguments) 0) read) bottlenose-ns)');
    my $result = `racket $tmpcode '(load "$tmpcmd")'`;
    chomp $result;
#    print "answer was ", $result, " ";
    return $result;
}

sub scm_equal {
    my ($e0, $e1) = @_;
    my $tmp = File::Temp->new;
    $tmp->say("(equal? $e0 $e1)");
    my $result = `racket -e '(load "$tmp")'`;
    chomp $result;
    return $result eq "#t";
}

sub scm_equal_sign {
    my ($e0, $e1) = @_;
    my $tmp = File::Temp->new;
    $tmp->say("(equal?/recur (list->vector $e0) (list->vector $e1) =)");
    my $result = `racket -e '(load "$tmp")'`;
    chomp $result;
    return $result eq "#t";
}

use Test::Simple tests => 14;

ok(scm_equal(scm_eval("exercises.rkt", "(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))"), "'(22 26 30)"), "(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))");
ok(scm_equal(scm_eval("exercises.rkt", "p2_1"), '#t'), "p2_1");
ok(scm_equal(scm_eval("exercises.rkt", "p2_2"), '#t'), "p2_2");
ok(scm_equal(scm_eval("exercises.rkt", "p2_3"), '#t'), "p2_3");
ok(scm_equal(scm_eval("exercises.rkt", "p2_4"), '#t'), "p2_4");
ok(scm_equal(scm_eval("exercises.rkt", "p2_5"), '#t'), "p2_5");
ok(scm_equal(scm_eval("exercises.rkt", "p2_6"), '#t'), "p2_6");
ok(scm_equal(scm_eval("exercises.rkt", "p2_7"), '#t'), "p2_7");
ok(scm_equal(scm_eval("exercises.rkt", "(my-equal? '(this is a list) '(this is a list))"), "#t"), "(my-equal? '(this is a list) '(this is a list))");
ok(scm_equal(scm_eval("exercises.rkt", "(my-equal? '(this is a list) '(this (is a) list))"), "#f"), "(my-equal? '(this is a list) '(this (is a) list))");
ok(scm_equal(scm_eval("exercises.rkt", "(my-equal? '(1 (2 3) 4) '(1 (2 3) 4))"), "#t"), "(my-equal? '(1 (2 3) 4) '(1 (2 3) 4))");
ok(scm_equal(scm_eval("exercises.rkt", "(my-equal? 'a 'b)"), "#f"), "(my-equal? 'a 'b)");
ok(scm_equal(scm_eval("exercises.rkt", "(my-equal? 'app 'app)"), "#t"), "(my-equal? 'app 'app)");
ok(scm_equal(scm_eval("exercises.rkt", "p4"), '#t'), "p4");
