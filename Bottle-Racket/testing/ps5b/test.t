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

use Test::Simple tests => 38;

ok(scm_equal(scm_eval("exercise2-58.rkt", "(make-sum-infix 'x 3)"), "'(x + 3)"), "infix make-sum constructor");
ok(scm_equal(scm_eval("exercise2-58.rkt", "(sum?-infix (make-sum-infix 'x 3))"), "#t"), "infix sum predicate");
ok(scm_equal(scm_eval("exercise2-58.rkt", "(addend-infix (make-sum-infix 'x 3))"), "'x"), "infix addend selector");
ok(scm_equal(scm_eval("exercise2-58.rkt", "(augend-infix (make-sum-infix 'x 3))"), "3"), "infix augend selector");

ok(scm_equal(scm_eval("exercise2-58.rkt", "(make-product-infix 'x 3)"), "'(x * 3)"), "infix make-product constructor");
ok(scm_equal(scm_eval("exercise2-58.rkt", "(product?-infix (make-product-infix 'x 3))"), "#t"), "infix product predicate");
ok(scm_equal(scm_eval("exercise2-58.rkt", "(multiplier-infix (make-product-infix 'x 3))"), "'x"), "infix multiplier selector");
ok(scm_equal(scm_eval("exercise2-58.rkt", "(multiplicand-infix (make-product-infix 'x 3))"), "3"), "infix multiplicand selector");

ok(scm_equal(scm_eval("exercise2-58.rkt", "(make-exponentiation-infix 'x 3)"), "'(x ** 3)"), "infix make-exponentiation constructor");
ok(scm_equal(scm_eval("exercise2-58.rkt", "(exponentiation?-infix (make-exponentiation-infix 'x 3))"), "#t"), "infix exponentiation predicate");
ok(scm_equal(scm_eval("exercise2-58.rkt", "(base-infix (make-exponentiation-infix 'x 3))"), "'x"), "infix exponentiation base selector");
ok(scm_equal(scm_eval("exercise2-58.rkt", "(exponent-infix (make-exponentiation-infix 'x 3))"), "3"), "infix exponentiation exponent selector");

ok(scm_equal(scm_eval("exercise2-58.rkt", "(deriv-infix '(x + 3) 'x)"), "1"), "(deriv-infix '(x + 3) 'x)");
ok(scm_equal(scm_eval("exercise2-58.rkt", "(deriv-infix '(x * (y * (x + 3))) 'x)"), "'((x * y) + (y * (x + 3)))"), "(deriv-infix '(x * (y * (x + 3))) 'x)");
ok(scm_equal(scm_eval("exercise2-58.rkt", "(deriv-infix '((x * y) * (x + 3)) 'x)"), "'((x * y) + (y * (x + 3)))"), "(deriv-infix '((x * y) * (x + 3)) 'x)");
ok(scm_equal(scm_eval("exercise2-58.rkt", "(deriv-infix '((x ** 3) + (x ** 2)) 'x)"), "'((3 * (x ** 2)) + (2 * x))"), "(deriv-infix '((x ** 3) + (x ** 2)) 'x)");

ok(scm_equal(scm_eval("exercise2-57.rkt", "(make-sum 'x 3)"), "'(+ x 3)"), "(make-sum 'x 3) is '(+ x 3)");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(make-sum 1 2)"), "3"), "(make-sum 1 2) is 3");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(make-sum 'x 0)"), "'x"), "(make-sum 'x 0) is 'x");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(make-sum 0 'x)"), "'x"), "(make-sum 0 'x) is 'x");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(make-sum 'x)"), "'x"), "(make-sum 'x) is 'x");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(make-sum 'x 'y 'z)"), "'(+ x y z)"), "(make-sum 'x 'y 'z) is '(+ x y z)");

ok(scm_equal(scm_eval("exercise2-57.rkt", "(addend '(+ x 3))"), "'x"), "(addend '(+ x 3)) is 'x");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(augend '(+ x 3))"), "3"), "(augend '(+ x 3)) is 3");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(addend '(+ x y z))"), "'x"), "(addend '(+ x y z)) is 'x");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(augend '(+ x y z))"), "'(+ y z)"), "(augend '(+ x y z)) is '(+ y z)");

ok(scm_equal(scm_eval("exercise2-57.rkt", "(make-product 'x 3)"), "'(* x 3)"), "(make-product 'x 3) is '(* x 3)");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(make-product 1 2)"), "2"), "(make-product 1 2) is 2");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(make-product 'x 1)"), "'x"), "(make-product 'x 1) is 'x");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(make-product 1 'x)"), "'x"), "(make-product 1 'x) is 'x");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(make-product 'x)"), "'x"), "(make-product 'x) is 'x");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(make-product 'x 'y 'z)"), "'(* x y z)"), "(make-product 'x 'y 'z) is '(* x y z)");

ok(scm_equal(scm_eval("exercise2-57.rkt", "(multiplier '(* x 3))"), "'x"), "(multiplier '(* x 3)) is 'x");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(multiplicand '(* x 3))"), "3"), "(multiplicand '(* x 3)) is 3");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(multiplier '(* x y z))"), "'x"), "(multiplier '(* x y z)) is 'x");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(multiplicand '(* x y z))"), "'(* y z)"), "(multiplicand '(* x y z)) is '(* y z)");

ok(scm_equal(scm_eval("exercise2-57.rkt", "(multiplicand equation1)"), "'(* y (+ x 3))"), "(multiplicand equation1) is correct");
ok(scm_equal(scm_eval("exercise2-57.rkt", "(deriv equation1 'x)"), "'(+ (* x y) (* y (+ x 3)))"), "(deriv equation1 'x) is correct");

