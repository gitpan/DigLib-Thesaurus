use ExtUtils::MakeMaker;
WriteMakefile(
	      'NAME'	     => 'DigLib::Thesaurus',
	      'VERSION_FROM' => 'Thesaurus.pm',
	      'PREREQ_PM'    => {
				 'DigLib::MLang' => '0.03',
				 'Storable' => '1.0006',
				},
	     );
