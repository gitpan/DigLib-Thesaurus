#!/usr/bin/perl -w
use DigLib::Thesaurus;
use Data::Dumper;

$thesaurus = thesaurusLoad('thesaurus');
print Dumper($thesaurus->depth_first("_top_",3,"NT","BT"));


