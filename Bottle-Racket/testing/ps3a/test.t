#!/usr/bin/perl
use 5.10.0;
use warnings FATAL => 'all';

use File::Temp;
use File::Copy;
use IO::Handle;

sub scm_eval {
    my ($cmdline) = @_;
    my $tmpcmd = File::Temp->new;
    $tmpcmd->say($cmdline);
    my $tmpcode = File::Temp->new;
    copy("ps3a.rkt", $tmpcode) or die "Copy failed: $!";
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

use Test::Simple tests => 15;

ok(scm_equal_sign(scm_eval("(midpoint-segment (make-segment (make-point 0 0) (make-point 2 2)))"), "'(1 1)"), "(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 2 2))))");
ok(scm_equal(scm_eval("(area-rect (make-rect (make-point 1 1) (make-point 3 7)))"), 12), "(area-rect (make-rect (make-point 1 1) (make-point 3 7)))");
ok(scm_equal(scm_eval("(perimeter-rect (make-rect (make-point 1 1) (make-point 3 7)))"), 16), "(perimeter-rect (make-rect (make-point 1 1) (make-point 3 7)))");
ok(scm_equal(scm_eval("(my-cdr (my-cons 14 15))"), '15'), "my-cdr");
ok(scm_equal(scm_eval("p4"), '#t'), "p5");
ok(scm_equal(scm_eval("(list-prod-iter '(1 2 3 4))"), 24), "(list-prod-iter '(1 2 3 4))");
ok(scm_equal(scm_eval("(double-list1 '(1 2 3))"), "'(2 4 6)"), "(double-list1 '(1 2 3))");
ok(scm_equal(scm_eval("(double-list2 '(1 2 3))"), "'(2 4 6)"), "(double-list2 '(1 2 3))");
ok(scm_equal(scm_eval("(double-list3 '(1 2 3))"), "'(2 4 6)"), "(double-list2 '(1 2 3))");
ok(scm_equal(scm_eval("(square-list1 '(1 2 3))"), "'(1 4 9)"), "(square-list1 '(1 2 3))");
ok(scm_equal(scm_eval("(square-list2 '(1 2 3))"), "'(1 4 9)"), "(square-list2 '(1 2 3))");
ok(scm_equal(scm_eval("(my-map square '(1 2 3))"), "'(1 4 9)"), "(my-map square '(1 2 3))");
ok(scm_equal(scm_eval("(my-append '(1 2) '(3 4))"), "'(1 2 3 4)"), "(my-append '(1 2) '(3 4))");
ok(scm_equal(scm_eval("(my-length '(1 2 3 4))"), 4), "(my-length '(1 2 3 4))");
ok(scm_equal(scm_eval("(count-leaves '(1 (2 3) 4 (5 6)))"), 6), "(count-leaves '(1 (2 3) 4 (5 6)))");
