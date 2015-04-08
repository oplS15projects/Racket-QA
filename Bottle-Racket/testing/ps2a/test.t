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
    copy("ps2a.rkt", $tmpcode) or die "Copy failed: $!";
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

use Test::Simple tests => 15;

ok(scm_equal(scm_eval("add"), '#t'), "add");
ok(scm_equal(scm_eval("(f-recursive 2)"), 2), "(f-recursive 2)");
ok(scm_equal(scm_eval("(f-recursive 3)"), 4), "(f-recursive 3)");
ok(scm_equal(scm_eval("(f-recursive 4)"), 11), "(f-recursive 4)");
ok(scm_equal(scm_eval("(f-iterative 2)"), 2), "(f-iterative 2)");
ok(scm_equal(scm_eval("(f-iterative 3)"), 4), "(f-iterative 3)");
ok(scm_equal(scm_eval("(f-iterative 5)"), 25), "(f-iterative 5)");
ok(scm_equal(scm_eval("tail-recursion"), '#t'), "tail-recursion");
ok(scm_equal(scm_eval("(fast-expt 2 0)"), 1), "(fast-expt 2 0)");
ok(scm_equal(scm_eval("(fast-expt 2 1)"), 2), "(fast-expt 2 1)");
ok(scm_equal(scm_eval("(fast-expt 2 2)"), 4), "(fast-expt 2 2)");
ok(scm_equal(scm_eval("(fast-expt 2 3)"), 8), "(fast-expt 2 3)");
ok(scm_equal(scm_eval("(fast-expt 2 5)"), 32), "(fast-expt 2 5)");
ok(scm_equal(scm_eval("(sum square 2 inc 4)"), 29), "(sum square 2 inc 4)");
ok(scm_equal(scm_eval("(sum square 2 inc 2)"), 4), "(sum square 2 inc 2)");

