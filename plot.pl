#!/usr/bin/perl

# Handle CGI

use strict;
use warnings;
use POSIX qw(strftime);
use IO::Handle;
use CGI qw(:standard);

my $system_up = 1;

my $root = '@DATADIR@';

my $cr = "\r\n";

if (!$system_up) {
  print header(), start_html(), h1("Sorry, Relplot is down for service"),
		end_html();
  exit 0;
}

sub quot {
    my $x = $_[0];
    if (!defined($x)) { return "\'\'" }
    $x =~ s/'/'\\''/g;
    return ' \''.$x.'\' ';
}

my $minx = param('minx') + 0.0;
my $maxx = param('maxx') + 0.0;
my $miny = param('miny') + 0.0;
my $maxy = param('maxy') + 0.0;
my $aspect = param('fixed_aspect');
my $hide_axes = param('hide_axes');
my $grid_lines = param('grid_lines');

my $numeqns = 0.0 + param("numeqns");
my @eqns, my @colors, my @lines, my @widths;

if ($numeqns < 1) {
    print header(), start_html(), "<p>invalid equations</p>";
    exit 1;
}

my $eqn;
my $j = 1;
for (my $i = 1; $i <= $numeqns; $i++, $j++) {
    $eqns[$j] = param('eqn' . $i); chomp $eqns[$j];
    if ($eqns[$j] =~ m/^(\s)*$/) {
	$j--;
	next; # skip blank equations
    }
    $colors[$j] = param('color' . $i);
    $lines[$j] = param('line' . $i);
    $widths[$j] = param('lw' . $i);

    if ($j != 1) { $eqn .= '; ' }
    $eqn .= $eqns[$j];
}
$numeqns = $j - 1;

if (!defined($aspect) || $aspect ne 'yes') { $aspect = 'no'; }
if (!defined($hide_axes) || $hide_axes ne 'yes') { $hide_axes = 'no'; }
if (!defined($grid_lines) || $grid_lines ne 'yes') { $grid_lines = 'no'; }

my $now = strftime "%a %b %e %H:%M:%S %Y", localtime;
print STDERR  "opening @DATADIR@/logs/requests\r\n";

if (-w "@DATADIR@/logs/requests") {
    open(LOG, ">>@DATADIR@/logs/requests");
} else {
    print STDERR "Cannot open @DATADIR@/logs/requests for writing\n";
}

my $remote_ip_address = http('HTTP_IPREMOTEADDR');
if (!defined($remote_ip_address)) {
    $remote_ip_address = http('HTTP_REMOTE_ADDRESS');
}
if (!defined($remote_ip_address)) {
    $remote_ip_address = remote_addr();
}

my $ref = param('referer');
if (!defined($ref)) { $ref = ''; }
print LOG "$now : $remote_ip_address $ref : $eqn\n";

LOG->flush();

my $script;
my $output = "ps";
if (defined(param('pdf'))) {
    print 'Content-Type: application/pdf; name="plot.pdf"',$cr;
    print 'Content-Transfer-Encoding: 8bit',$cr;
    print 'Content-Disposition: inline; filename="plot.pdf"',$cr, $cr;
    $output = "pdf";
} else {
    print 'Content-Type: application/postscript; name="plot.ps"',$cr;
    print 'Content-Disposition: inline; filename="plot.ps"',$cr,$cr;
    $output = "ps";
}
$script = "@CGIBINDIR@/relplot.pl";

my @args = ("@EXECDIR@/timeout", 60, '--', $script, $output, $aspect, $hide_axes, $grid_lines, $minx, $maxx, $miny, $maxy, $numeqns);
#my @args = ($script, $output, $aspect, $hide_axes, $grid_lines, $minx, $maxx, $miny, $maxy, $numeqns);

for (my $i = 1; $i <= $numeqns; $i++) {
    push @args, $eqns[$i], $colors[$i], $lines[$i], $widths[$i];
}

chdir("@DATADIR@");

my $scmd = "";

foreach my $arg (@args) {
    $scmd .= &quot($arg);
}

# print LOG 'About to call system: ', $scmd, $cr;
# close LOG;
# system {"@EXECDIR@/timeout"} @args;
# system {"$script"} @args;
#
# Don't know why I can't simply call using system (or exec) --
# somehow the stdout file descriptor does not seem to work across
# the exec.

# clear the environment because of Shellshock
foreach my $v (keys %ENV) {
    if ($v ne "PATH") {
	delete $ENV{$v};
    }
}

open (PLOT, $scmd.' |');
undef $/;
while (<PLOT>) {
    #print LOG "Result line: $_";
    print $_;
}
close(PLOT);
close(LOG);
