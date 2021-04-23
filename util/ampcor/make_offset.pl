#!/usr/bin/perl
### make_offset.pl

$] >= 5.004 or die "Perl version must be >= 5.004 (Currently $]).\n";

use Env qw(INT_SCR INT_BIN);
use lib "$INT_SCR";  #### Location of Generic.pm
use Generic;

###Usage info/check

sub Usage{

`$INT_SCR/pod2man.pl  $INT_SCR/make_offset.pl`;
exit 1;
}
@ARGV >= 4 or Usage();
@args = @ARGV;

$date1         = shift;
$date2         = shift;
$baseline_file = shift;
$BaselineType  = shift;
$x_start       = shift;
$y_start       = shift;
$MPI_PARA      = shift;
$NUM_PROC      = shift;
$ROMIO         = shift;
$infile1       = "$date1.slc";
$infile2       = "$date2.slc";
$outfile       = "$date1-${date2}_ampcor.off";

#################
Message "Checking I/O";
#################
@Infiles  = ($infile1, $infile2, "$infile1.rsc", "$infile2.rsc");
@Outfiles = ($outfile, "$outfile.rsc");
&IOcheck(\@Infiles, \@Outfiles);
Log("make_offset.pl", @args);

##########################################
Message "Reading resource file: $baseline_file";
##########################################
$TimeSpan = Use_rsc "$baseline_file read TIME_SPAN_YEAR";
Use_rsc "$outfile write TIME_SPAN_YEAR $TimeSpan";

$search_max = 500; # was 64 before EJF 2007/8/20

$azpix1 = Use_rsc "$infile1 read AZIMUTH_PIXEL_SIZE";
$azpix2 = Use_rsc "$infile2 read AZIMUTH_PIXEL_SIZE";
$r_sfy = $azpix1/$azpix2;

Use_rsc "$outfile write TIME_SPAN_YEAR $TimeSpan";

if (($x_start!=0.01) or ($y_start!=0.01)){
  $range_offset   = $x_start;
  $azimuth_offset = $y_start;
}
else{
  $range_offset   = int(Use_rsc "$baseline_file read ORB_SLC_R_OFFSET_${BaselineType}");
  $azimuth_offset = int(Use_rsc "$baseline_file read ORB_SLC_AZ_OFFSET_${BaselineType}");
# increased to 20 azimuth samples EJF 2005/11/16
  $file = "$date1-${date2}_ampcor_gross";
  `$INT_SCR/offset.pl $infile1 \\
                      $infile2 \\
                      $file \\
                      2 \\
                      cpx \\
                      $range_offset \\
                      $azimuth_offset \\
                      1. $r_sfy \\
                      20 20 128 $search_max \\
                      $MPI_PARA       \\
                      $NUM_PROC       \\
                      $ROMIO      `;

  Status "make_offset.pl";
  $origfile = "$date1-${date2}_ampcor_gross.off";
  $cullfile = "$date1-${date2}_cull_gross.off";
  $dumpfile = "fitoff_ampcor_gross.out";
  `cp $origfile.rsc $cullfile.rsc`;
  Message "$INT_BIN/fitoff $origfile $cullfile 1.5 0.5 10 > $dumpfile";
  `$INT_BIN/fitoff $origfile $cullfile 1.5 0.5 10 > $dumpfile`;

  ### Check to make sure number of culled points is greater than 10
  open CULL, "$cullfile";
  for ($i=1; $line = <CULL>; $i++){}
  close(CULL);
  Message "$i points left after culling\n";

  ### If too few points, try again with different baseline estimates
  if ($i < 10)  {
    Message "Too few points after using range offset $range_offset and azimuth offset $azimuth_offset, try values 0.01&0.01";  
    $range_offset   = 0.01;
    $azimuth_offset = 0.01;
    $file = "$date1-${date2}_ampcor_gross";
    `rm $file.off $file.in $file.out`;
    `$INT_SCR/offset.pl $infile1 \\
                        $infile2 \\
                        $file \\
                        2 \\
                        cpx \\
                        $range_offset \\
                        $azimuth_offset \\
                        1. $r_sfy \\
                        20 20 128 $search_max \\
                        $MPI_PARA       \\
                        $NUM_PROC       \\
                        $ROMIO      `;
    Status "make_offset.pl";
    Message "$INT_BIN/fitoff $origfile $cullfile 1.5 0.5 10 > $dumpfile";
    `$INT_BIN/fitoff $origfile $cullfile 1.5 0.5 10 > $dumpfile`;
    open CULL, "$cullfile";
    for ($i=1; $line = <CULL>; $i++){}
    close(CULL);
    Message "$i points left after culling\n";
    $i > 10 or die "Still too few points left after culling with 0.01/0.01 offsets : $i left\n";
    Message "$i points left after culling\n";
  }
  open CULL, "$cullfile" or 
  die "Can't open $cullfile\n";
  open GX, ">gross_x" or die "Can't write to gross_x\n";
  open GY, ">gross_y" or die "Can't write to gross_y\n";
  while (<CULL>){
    @line = split /\s+/, $_;
    push @X, $line[2];
    push @Y, $line[4]; 
  }
  close (CULL);
  @X = sort @X;
  @Y = sort @Y;
  foreach (@X) {print GX "$_\n";}
  foreach (@Y) {print GY "$_\n";}
  $gox= Median(\@X);
  $goz= Median(\@Y);
  $gox or $gox = 0;
  $goz or $goz = 0;
  $range_offset   = int($gox);
  $azimuth_offset = int($goz);
}

#####################################
Message "extracting affine transformation parameters and registering simulation ";
#####################################
($m11, $m12, $m21, $m22, $t1, $t2) = split /\s+/, `$INT_SCR/find_affine.pl $dumpfile`;
Status "find_affine.pl";
$range_offset   = int($t1);
$azimuth_offset = int($t2);
$r_sfx = $m11;
$r_sfy = $m22;

##################################
Message "Fine Culling";
##################################
$file = "$date1-${date2}_ampcor";

`$INT_SCR/offset.pl $infile1 \\
                    $infile2 \\
                    $file \\
                    1 \\
                    cpx \\
                    $range_offset \\
                    $azimuth_offset \\
                    $r_sfx $r_sfy \\
                    30 30 64 8      \\
                    $MPI_PARA       \\
                    $NUM_PROC       \\
                    $ROMIO      `;
Status "offset.pl";

$origfile = "$date1-${date2}_ampcor.off";
$cullfile = "$date1-${date2}_cull.off";
$dumpfile = "fitoff_ampcor.out";
`cp $origfile.rsc $cullfile.rsc`;

Message "$INT_BIN/fitoff $origfile $cullfile 1.5 0.08 50 > $dumpfile";
`$INT_BIN/fitoff $origfile $cullfile 1.5 0.08 50 > $dumpfile`;

### Check to make sure number of culled points is greater than 50
open CULL, "$cullfile";
for ($i=1; $line = <CULL>; $i++){}
close(CULL);
$i > 50 or die "Too few points left after culling: $i left\n";
print "$i points left after culling\n";

exit 0;

=pod

=head1 USAGE

B<make_offset.pl> I<date1 date2 base_rsc [x_start y_start]>

 date1: the SLC is I<date1>.slc
 date2: the SLC is I<date2>.slc
 base_rsc: rscfile produced by baseline.pl
 x_start: range offset in pixels between images
 y_start: azimuth offset in pixels between images

Uses I<base_rsc> to calculate starting offset unless I<x_start> or I<y_start> are entered

=head1 FUNCTION

Computes the offset field between 2 SLC

=head1 ROUTINES CALLED

offset.pl

fitoff

=head1 CALLED BY

raw2ampintcor.pl 

=head1 FILES USED

I<date1>.slc

I<date1>.slc.rsc

I<date2>.slc

I<date2>.slc.rsc

=head1 FILES CREATED

I<date1>-I<date2>_ampcor_gross.in

I<date1>-I<date2>_ampcor_gross.out

I<date1>-I<date2>_ampcor_gross.off

I<date1>-I<date2>_ampcor_gross.off.rsc

I<date1>-I<date2>_ampcor.in

I<date1>-I<date2>_ampcor.out

I<date1>-I<date2>_ampcor.off

I<date1>-I<date2>_ampcor.off.rsc

$date1-${date2}_cull_gross.off

fitoff_ampcor_gross.out

gross_x

gross_y

cull_gross.out

=head1 HISTORY

Shell Script : Francois ROGEZ 96/98

Perl  Script : Rowena LOHMAN 04/18/98

Frederic CRAMPE, Jan 11, 2000

=head1 LAST UPDATE

Eric Fielding, Aug. 20, 2007

=cut
