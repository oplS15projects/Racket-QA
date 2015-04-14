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
    copy("ps3b.rkt", $tmpcode) or die "Copy failed: $!";
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

use Test::Simple tests => 7;

ok(scm_equal(scm_eval("(same-parity 1 2 3 4 5 6 7)"), "'(1 3 5 7)"), "(same-parity 1 2 3 4 5 6 7)");
ok(scm_equal(scm_eval("(same-parity 2 3 4 5 6 7)"), "'(2 4 6)"), "(same-parity 2 3 4 5 6 7)");
ok(scm_equal(scm_eval("(reverse a)"), "'((3 (4 5)) (1 2))"), "(reverse a)");
ok(scm_equal(scm_eval("(deep-reverse a)"), "'(((5 4) 3) (2 1))"), "(deep-reverse a)");
ok(scm_equal(scm_eval("((one inc) 0)"), '1'), "Church one");
ok(scm_equal(scm_eval("((two inc) 0)"), '2'), "Church two");
ok(scm_equal(scm_eval("(((add one two) inc) 0)"), '3'), "Church add");
