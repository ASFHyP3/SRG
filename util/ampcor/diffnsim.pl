#!/usr/bin/perl
### diffnsim.pl

$] >= 5.004 or die "Perl version must be >= 5.004 (Currently $]).\n";

use Env qw(INT_SCR INT_BIN);
use lib "$INT_SCR";  #### Location of Generic.pm
use Generic;

###Usage info/check
sub Usage{

`$INT_SCR/pod2man.pl  $INT_SCR/diffnsim.pl`;
exit 1;
}
@ARGV == 8 or Usage();
@args = @ARGV;

$intfile   = shift;
$hgtfile   = shift;
$diffile   = shift;
$synthfile = shift;
$nlook     = shift;
$baseline_file= shift;
$BaselineType = shift;
$applymask    = shift;

#################
Message "Checking I/O";
#################
@Infiles  = ($intfile, "$intfile.rsc", 
	     $hgtfile,"$hgtfile.rsc",
	     $baseline_file); 

@Outfiles = ($diffile, "$diffile.rsc", 
	     $synthfile, "$synthfile.rsc");

&IOcheck(\@Infiles, \@Outfiles);
Log("diffnsim.pl", @args);

##########################################
Message "Reading resource file: $intfile.rsc";
##########################################
$infile = $intfile;

$width              = Use_rsc "$infile read WIDTH";
$length             = Use_rsc "$infile read FILE_LENGTH";
$xmin               = Use_rsc "$infile read XMIN";
$xmax               = Use_rsc "$infile read XMAX";
$ymin               = Use_rsc "$infile read YMIN";
$ymax               = Use_rsc "$infile read YMAX";
$height_top         = Use_rsc "$infile read HEIGHT";
$height_rate        = Use_rsc "$infile read HEIGHT_DS";
$height_acc         = Use_rsc "$infile read HEIGHT_DDS";
$earth_radius       = Use_rsc "$infile read EARTH_RADIUS";
$starting_range1    = Use_rsc "$infile read STARTING_RANGE1";
$starting_range2    = Use_rsc "$infile read STARTING_RANGE2";
$wavelength         = Use_rsc "$infile read WAVELENGTH";
$range_pixel_size   = Use_rsc "$infile read RANGE_PIXEL_SIZE";
$azimuth_pixel_size = Use_rsc "$infile read AZIMUTH_PIXEL_SIZE";
$rlks               = Use_rsc "$infile read RLOOKS";
$alks               = Use_rsc "$infile read ALOOKS";
$first_line_utc     = Use_rsc "$infile read FIRST_LINE_UTC";
$center_line_utc    = Use_rsc "$infile read CENTER_LINE_UTC";
$delta_line_utc     = Use_rsc "$infile read DELTA_LINE_UTC";
$slc_ref_line       = Use_rsc "$infile read SLC_RELATIVE_YMIN";
$squint             = Use_rsc "$infile read SQUINT";
$velocity           = Use_rsc "$infile read VELOCITY";
$antenna_side       = Use_rsc "$infile read ANTENNA_SIDE";
$prf                = Use_rsc "$infile read PRF";
$fs                 = Use_rsc "$infile read RANGE_SAMPLING_FREQUENCY";
$dop0               = Use_rsc "$infile read DOPPLER_RANGE0";
$dop1               = Use_rsc "$infile read DOPPLER_RANGE1";
$dop2               = Use_rsc "$infile read DOPPLER_RANGE2";
$dop3               = Use_rsc "$infile read DOPPLER_RANGE3";

($h_baseline_top, $h_baseline_rate, $h_baseline_acc, $v_baseline_top, $v_baseline_rate, $v_baseline_acc) =
    split /\n/, `$INT_SCR/select_baseline.pl $baseline_file $BaselineType`;

$range_offset    = Use_rsc "$baseline_file read RANGE_OFFSET_${BaselineType}";
$phase_const     = Use_rsc "$baseline_file read PHASE_CONST_${BaselineType}";
if ($phase_const == -99999) {$phase_const = 0.;}
else {$phase_const=-$phase_const;}

##########################################
Message "Reading resource file: $hgtfile.rsc";
##########################################
$heading_deg              = Use_rsc "$hgtfile read HEADING_DEG";
$range_ref[0]             = Use_rsc "$hgtfile read RGE_REF1";
if ($range_ref[0] != 0)
 {
 $look_ref[0]              = Use_rsc "$hgtfile read LOOK_REF1";
 $lat_ref[0]               = Use_rsc "$hgtfile read LAT_REF1";
 $lon_ref[0]               = Use_rsc "$hgtfile read LON_REF1";
 $range_ref[1]             = Use_rsc "$hgtfile read RGE_REF2";
 $look_ref[1]              = Use_rsc "$hgtfile read LOOK_REF2";
 $lat_ref[1]               = Use_rsc "$hgtfile read LAT_REF2";
 $lon_ref[1]               = Use_rsc "$hgtfile read LON_REF2";
 $range_ref[2]             = Use_rsc "$hgtfile read RGE_REF3";
 $look_ref[2]              = Use_rsc "$hgtfile read LOOK_REF3";
 $lat_ref[2]               = Use_rsc "$hgtfile read LAT_REF3";
 $lon_ref[2]               = Use_rsc "$hgtfile read LON_REF3";
 $range_ref[3]             = Use_rsc "$hgtfile read RGE_REF4";
 $look_ref[3]              = Use_rsc "$hgtfile read LOOK_REF4";
 $lat_ref[3]               = Use_rsc "$hgtfile read LAT_REF4";
 $lon_ref[3]               = Use_rsc "$hgtfile read LON_REF4";
 }

############################################
Message "Writing resource file: $diffile.rsc";
############################################
$new_width   = int($width/$nlook);
$new_length  = int($length/$nlook);
$new_xmin    = int($xmin/$nlook);
$new_xmax    = int($xmax/$nlook);
$new_ymin    = int($ymin/$nlook);
$new_ymax    = int($ymax/$nlook);
$new_rng_pix = $range_pixel_size*$nlook;
$new_az_pix  = $azimuth_pixel_size*$nlook;
$new_dt      = $delta_line_utc*$nlook;
$new_rlks    = $rlks*$nlook;
$new_alks    = $alks*$nlook;

Use_rsc "$diffile write WIDTH              $new_width";
Use_rsc "$diffile write FILE_LENGTH        $new_length";
Use_rsc "$diffile write XMIN               $new_xmin";
Use_rsc "$diffile write XMAX               $new_xmax";
Use_rsc "$diffile write YMIN               $new_ymin";
Use_rsc "$diffile write YMAX               $new_ymax ";
Use_rsc "$diffile write RANGE_PIXEL_SIZE   $new_rng_pix";
Use_rsc "$diffile write AZIMUTH_PIXEL_SIZE $new_az_pix";
Use_rsc "$diffile write RLOOKS             $new_rlks";
Use_rsc "$diffile write ALOOKS             $new_alks";
Use_rsc "$diffile write DELTA_LINE_UTC     $new_dt";
Use_rsc "$diffile write BASELINE_SRC       $BaselineType";
Doc_rsc(
 RSC_Tip => 'Baseline Orbit Type',
 RSC_Doc => q[
   Based on comments in state_vector.pl
   orbit_type: PRC (precision orbit) or ODR (Delft)
               or HDR (state vectors in header)
   ],
 RSC_Derivation => q[
   Initial value specified in TEST_DIR/int.proc via
     OrbitType=PRC
   which becomes $OrbitType in process_2pass.pl
   which is passed as positional parameter 15 to raw2ampintcor.pl
   which becomes $OrbitType in raw2ampintcor.pl
   which is passed as positional parameter 7 to diffnsim.pl
   which becomes $BaselineType in diffnsim.pl

   process_2pass.pl runs diffnsim.pl directly
   passing the constant 'SIM'
   as positional parameter 7 to diffnsim.pl
   which becomes $BaselineType in diffnsim.pl
   ],
 RSC_Comment => q[
   Keyword does not appear to be read from resource file anywhere.

   However $BaselineType is used various places in construction
   of keyword names.
   $OrbitType is used various places in construction of filenames.
   ],
 RSC_Type => String,
 RSC_Type => 'One of {ODR,PRC,HDR,SIM}',
);



if ($range_ref[0] != 0)
 {
Use_rsc "$diffile write HEADING_DEG  $heading_deg";
Use_rsc "$diffile write RGE_REF1     $range_ref[0]";
Use_rsc "$diffile write LOOK_REF1    $look_ref[0]";
Use_rsc "$diffile write LAT_REF1     $lat_ref[0]";
Use_rsc "$diffile write LON_REF1     $lon_ref[0]";
Use_rsc "$diffile write RGE_REF2     $range_ref[1]";
Use_rsc "$diffile write LOOK_REF2    $look_ref[1]";
Use_rsc "$diffile write LAT_REF2     $lat_ref[1]";
Use_rsc "$diffile write LON_REF2     $lon_ref[1]";
Use_rsc "$diffile write RGE_REF3     $range_ref[2]";
Use_rsc "$diffile write LOOK_REF3    $look_ref[2]";
Use_rsc "$diffile write LAT_REF3     $lat_ref[2]";
Use_rsc "$diffile write LON_REF3     $lon_ref[2]";
Use_rsc "$diffile write RGE_REF4     $range_ref[3]";
Use_rsc "$diffile write LOOK_REF4    $look_ref[3]";
Use_rsc "$diffile write LAT_REF4     $lat_ref[3]";
Use_rsc "$diffile write LON_REF4     $lon_ref[3]";
 }

Use_rsc "$diffile merge $intfile";

`cp $diffile.rsc $synthfile.rsc`;
`rm -f $synthfile`;
`rm -f $diffile`;

$LeftorRight = "Right" ;
if($antenna_side == 1) 
  {
    $LeftorRight = "Left" ;
  }

########################
Message "Running diffnsim";
########################
open DIFF, ">diffnsim_$diffile.in" or die "Can't write to diffnsim_$diffile.in: $!\n";
print DIFF <<END;
Ramped input interferogram                    (-)  =  $intfile    ! unflattened phase
Differential output interferogram             (-)  =  $diffile    ! flattened output
DEM in radar coordinates                      (-)  =  $hgtfile    ! DEM in radar coordinates
Simulated output DEM interferogram            (-)  =  $synthfile  ! phase used to flatten
Number of pixels down, across               (-,-)  = $length $width  ! pixels down and across
Number of looks to take down, across        (-,-)  = $nlook $nlook   ! looks
Cross Track Baseline, Rate & Acceleration (m,-,-)  = $h_baseline_top $h_baseline_rate $h_baseline_acc  ! m, m/m, m/m^2
Vertical Baseline, Rate, & Acceleration   (m,-,-)  = $v_baseline_top $v_baseline_rate $v_baseline_acc  ! m, m/m, m/m^2
Phase Offset                                  (-)  = $phase_const    ! offset (radians)                        
Platform Altitude, Rate, & Acceleration   (m,-,-)  = $height_top $height_rate $height_acc   ! h0,hdot,hddt  !(m),(m/m),(m/m^2)
SLC Relative Line Offset                      (-)  = $slc_ref_line  ! offset from slc start line
Radar Wavelength                              (m)  = $wavelength     !
Earth Radius                                  (m)  = $earth_radius   !
Starting Ranges for SLCs                    (m,m)  = $starting_range1 $starting_range2 ! 
Left or Right Looking                         (-)  = $LeftorRight        ! Left or Right
Range/Azimuth Pixel Size                    (m,m)  = $range_pixel_size  $azimuth_pixel_size  ! 
Specify Squint or Doppler Polynomial          (-)  = Doppler Polynomial       ! Squint/Doppler Polynomial
Doppler Cubic Polynomial                (-,-,-,-)  = $dop0 $dop1 $dop2 $dop3  ! fit versus range bin
Platform Velocity                           (m/s)  = $velocity    !
Radar PRF                                     (Hz)  = $prf         ! Hz
Radar Range Sampling Frequency                (Hz)  = $fs          ! Hz
END
    close(DIFF);

`$INT_BIN/diffnsim  diffnsim_$diffile.in`;
Status "diffnsim";

if ($applymask eq "y"){
### put $diffile & $synthfile phase values = 0 where no data 
### are available for $hgtfile (where amp($hgtfile)==0)
  `$INT_BIN/rmg2mag_phs  $hgtfile pwr_ref phs_ref $new_width`;
  `$INT_BIN/cpx2mag_phs  $diffile pwr phs $new_width`;
  `$INT_BIN/add_phs      phs pwr_ref phs_msk $new_width $new_length 0 1`;
  `$INT_BIN/mag_phs2cpx  pwr phs_msk $diffile $new_width`;
  `$INT_BIN/rmg2mag_phs  $synthfile pwr phs $new_width`;
  `$INT_BIN/add_phs      phs pwr_ref phs_msk $new_width $new_length 0 1`;
  `$INT_BIN/mag_phs2rmg  pwr phs_msk $synthfile $new_width`;
  `rm pwr phs pwr_ref phs_ref phs_msk`;
}elsif ($applymask eq "n"){
  Message "no masking applied";
}else{
  Message "mask flag must be y|n";
  exit 1;
}

exit 0;

=pod

=head1 USAGE

B<diffnsim.pl> I<intfile hgtfile diffile synthfile nlook baseline_file baseline_type maskflag>

Intfile = cpx, other files = rmg format

=head1 FUNCTION

Removes a synthetic phase from an interferogram and takes some looks

=head1 ROUTINES CALLED

diffnsim

=head1 CALLED BY

process.pl

=head1 FILES USED

I<intfile>

I<hgtfile>

I<intfile>.rsc

I<hgtfile>.rsc

I<baseline_file> 

=head1 FILES CREATED

I<diffile>

I<synthfile>

I<diffile>.rsc

I<synthfile>.rsc

diffnsim_<diffile>.in

=head1 HISTORY

Shell Script : Francois ROGEZ 96/98
Perl  Script : Rowena LOHMAN 04/18/98

=head1 LAST UPDATE

Frederic Crampe, Aug 19, 1999

=cut
