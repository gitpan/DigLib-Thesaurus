# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..3\n"; }
END {print "not ok 1\n" unless $loaded;}
use DigLib::Thesaurus;
use Data::Dumper;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

$thesaurus = thesaurusLoad('examples/thesaurus');
#$a = $thesaurus->full_dt( -default => sub{ "\n$class={". join(",",@terms)."}"});
#print $a;

###################### tc 

@ft= $thesaurus->tc("local","NT");

if(@ft == 10){ print "ok 2\n";}
else         { print "not ok 2\n"}

###################### depth_first

$ft= $thesaurus->depth_first("local" , 2 ,"NT","INST");
# print Dumper($ft);
$k=keys(%$ft);

if($k == 10-1){ print "ok 3\n";}
else         { print "not ok 3\n"}

