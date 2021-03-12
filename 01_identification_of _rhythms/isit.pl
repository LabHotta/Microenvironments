#!/bin/perl -w
use strict;
#pega lista de genes circadianos e obtem a fase dos circadianos


my $circadianos = ();
my @output = ();
my $output = ();
my @output_final = ();
my $output_final = ();
my $query = ();

#pegar a lista de genes circadianos
open(MYFILE, "rhythmic_modules.txt");              # Open the file
my @circadianos = <MYFILE>;              # Read it into an array
close(MYFILE);

open(MYFILE2, "all_probes_modules.txt");              # Open the file
my @total2 = <MYFILE2>;              # Read it into an array
close(MYFILE2);

#puxar uma linha

my $b = 0;
while ($b < @circadianos)
{ 
$query = $circadianos[$b];
chomp $query;
@output = grep(/$query/i, @total2);
push (@output_final, @output);
$b++;
};


open(MYFILE3, ">>final_probes_modules.txt");              # Open the file
print MYFILE3 "@output_final";
close(MYFILE3);

#pegar os dados de expressão da linha