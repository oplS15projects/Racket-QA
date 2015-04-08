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
    copy("ps2b.rkt", $tmpcode) or die "Copy failed: $!";
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

use Test::Simple tests => 8;

ok(scm_equal(scm_eval("(* (product1 pi-term 1 next 6) 4)"), '4096/1225'), "(* (product1 pi-term 1 next 6) 4)");
ok(scm_equal(scm_eval("(product2 pi-term 1 next 6)"), '1024/1225'), "(product2 pi-term 1 next 6)");
ok(scm_equal(scm_eval("(sum1 square 2 inc 4)"), 29), "(sum square 2 inc 4)");
ok(scm_equal(scm_eval("(* (product pi-term 1 next 6) 4)"), '4096/1225'), "(* (product pi-term 1 next 6) 4)");
ok(scm_equal(scm_eval("(((double (double double)) inc) 5)"), 21), "(((double (double double)) inc) 5)");
ok(scm_equal(scm_eval("((compose square inc) 6)"), 49), "((compose square inc) 6)");
ok(scm_equal(scm_eval("((expnt-iter 2) 3)"), 9), "((expnt-iter 2) 3)");
ok(scm_equal(scm_eval("((repeat square 2) 5)"), 625), "((repeat square 2) 5)");
