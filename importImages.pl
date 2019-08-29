#!/usr/bin/perl

use strict;
use warnings;
use diagnostics;
use DBI;
use Data::Dumper;

# DB Name.
my $db_name = "OASIS3.db";
# Directory with all the t1-w nifti files.
my $t1w_dir = "/home/donbibi/Documents/FYP/t1w/";

# Read in nifti files from the t1w_dir into an array.
opendir(my $dh, $t1w_dir) or die "Can't open $t1w_dir: $!";
my @t1w_files_unsorted = grep(/^.+\..+$/, readdir($dh));
closedir($dh);

my @t1w_files = sort @t1w_files_unsorted;
# Uncomment for debugging.
#print join("\n", @t1w_files),"\n";


# Connect to DB; assuming the SQLite DB is in the same directory as this perl script.
my $dbh = DBI-> connect(
	"dbi:SQLite:dbname=$db_name",
	{ RaiseError => 1 }
) or die $DBI::errstr;

# Process nifti files based on name, days since entry and runs.
my $subj_id;
foreach (@t1w_files)
{	
	my $substr = substr($_, 4, 8); # Obtain the subject id from file name, i.e. 'OAS30001'.
	if (!defined $subj_id || $subj_id ne $substr)
	{
		$subj_id = $substr; 
		process_nifti($_, $substr); # Obtain Clinical IDs for the new subject and process nifti.
	}
	else 
	{
		process_nifti($_); # Process nifti for existing set of Clinical IDs of a subject.
	}
}


# Array to store array of Clinical IDs per subject.
my @clinical_ids;

# Hash to store 'ADRC_ADRCCLINICALDATA ID' : 'date'.
my %clinical_hash;

# Subroutine for processing nifti files based on file name.
sub process_nifti
{
	my ($file_name, $id) = @_;
	
	if (defined $id)
	{
		# Clear array.
		undef @clinical_ids;
		# Clear hash.
		undef %clinical_hash;

		# READ cell values from 'ADRC_ADRCCLINICALDATA ID' column in DB for the given subject id into an array.
		my $sth_read = $dbh -> prepare("SELECT `ADRC_ADRCCLINICALDATA ID`
			FROM `subjects` WHERE `Subject` = \"$id\"");

		$sth_read -> execute() or die $DBI::errstr;
		$sth_read -> bind_columns(\my $val1);

		while ($sth_read -> fetch)
		{
			push @clinical_ids, $val1; # E.g. value OAS30001_ClinicalData_d0339.
		}

		$sth_read -> finish();

		# Create hash for 'ADRC_ADRCCLINICALDATA ID' : 'date', e.g. 'OAS30001_ClinicalData_d0339' : '0339'.
		foreach (@clinical_ids)
		{
			if ($_ =~ /OAS\d*_ClinicalData_d(\d*)/)
			{
				$clinical_hash{$_} = $1;
			}
		}

		# Uncomment for debugging.
		#print Dumper(\%clinical_hash);
	}

	my ($session_date, $run, $clinical_id, $sth_write);

	# Retrieve date and run.
	if ($file_name =~ /sub-OAS\d*_ses-d(\d*)_run-0(\d*)/)
	{
		$session_date = int($1);
		$run = $2;
	}

	print "$file_name \n";
	print "$session_date \n";

	# Criteria for deciding image : clinical ID.
	my ($date, $diff, $clin_key, $clin_date);
	while (($clin_key, $clin_date) = each (%clinical_hash))
	{
		if (int($clin_date) >= $session_date)
		{
			if (!defined $date || (int($clin_date) - $session_date) < $diff)
			{
				$date = int($clin_date);
				$diff = $date - $session_date;
				$clinical_id = $clin_key;
			}
		}
	}	
	print Dumper(\"session id $clinical_id, $file_name");

	# Open and read nifti.gz
	my $file_path = $t1w_dir . $file_name;
	open my $NIFTI, '<', $file_path  or die "Couldn't open NIFTI $t1w_dir $file_name, $!";

	my ($nifti_image, $buffer);
	while (read $NIFTI, $buffer, 1024)
	{
		$nifti_image .= $buffer;
	}

	if (defined $run)
	{
		$sth_write = "UPDATE `subjects` SET `run$run` = (?) WHERE `ADRC_ADRCCLINICALDATA ID` = \"$clinical_id\"";
	}
	else
	{
		$sth_write = "UPDATE `subjects` SET `run1` = (?) WHERE `ADRC_ADRCCLINICALDATA ID` = \"$clinical_id\"";
	}

	my $stm = $dbh -> prepare($sth_write);
	$stm -> bind_param(1, $nifti_image, DBI::SQL_BLOB);
	$stm -> execute();
	close($NIFTI);
	$stm -> finish();
}

# Close DB connection.
$dbh -> disconnect();


