package DigLib::Thesaurus;

require 5.6.0;
use strict;
use warnings;
require Exporter;
use Storable;
use DigLib::MLang;

# Module Stuff
our @ISA = qw(Exporter);
our %EXPORT_TAGS = ( 'all' => [ qw() ] );
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our @EXPORT = qw(&thesaurusLoad &thesaurusNew &thesaurusRetrieve
                 @terms $term $class);

our ($class,@terms,$term,$_corres );

# Version
our $VERSION = '0.10';

# Multi-language stuff
our $lang;

$INC{'DigLib/Thesaurus.pm'} =~ m!/Thesaurus\.pm$!;
$lang = loadMLangFile("$`/Thesaurus.lang");
$lang->setLanguage('pt');

sub terms {
  my ($self,$term,@rels) = @_;
  return map {if (exists($self->{thesaurus}->{$term}->{$_}))
		{ @{$self->{thesaurus}->{$term}->{$_} }}
	      else {()}} @rels;
}

sub external {
  my ($self,$term,$external) = @_;
  $external = uc($external);
  return $self->{thesaurus}->{$term}->{$external};
}

sub all_terms {
  my $self = shift;
  return sort keys %{$self->{thesaurus}};
}

sub depth_first {
  my ($self,$term,$niveis,@relat) = @_;
  my %st=();

  if ($niveis>=1) {
    for ($self->terms($term,@relat)) {
      $st{$_}=depth_first($self,$_,$niveis-1,@relat);
    }
    \%st; }
  elsif($niveis == 0) {1}
  else {1}
}

sub default_norelations {
  return { 'URL'=> 1, 'SN' => 1 };
}

sub default_inversions {
  return {
	  'NT' => 'BT',
	  'BT' => 'NT',
	  'USE' => 'UF',
	  'UF' => 'USE',
	 };
}

sub translateTerm {
  my ($self,$term,$lang) = @_;
  $term = lc($term);
  if ($lang) {
    my $trad;
    $lang = uc($lang);
    # Se foi $lang definido como linguagem
    if (defined($self->{languages}{$lang})) {
      # Se existe a tradução
      if (defined($trad = $self->{thesaurus}{$term}{$lang})) {
	return $trad;
      } else {
	return $self->{thesaurus}{$term}{_NAME_};
      }
    } else {
      return $self->{thesaurus}{$term}{_NAME_};
    }
  } else {
    return $self->{thesaurus}{$term}{_NAME_};
  }
}

sub top {
  my $self = shift;
  my $script = shift;
  return "<ul>".join("\n",
		     map {"<li><a href=\"$script?t=$_\">$_</a></li>"}
		     @{$self->{thesaurus}->{$self->{name}}->{NT}}). "</ul>";
}

sub default_descriptions {
  return {
	  'RT'  => $lang->get("rterm"),
	  'TT'  => $lang->get("tterm"),
	  'NT'  => $lang->get("nterm"),
	  'BT'  => $lang->get("bterm"),
	  'USE' => $lang->get("useterm"),
	  'UF'  => $lang->get("ufterm"),
	  'SN'  => $lang->get("snote"),
	 };
}

sub interfaceLanguages {
  return $lang->languages();
}

sub interfaceSetLanguage {
  $lang->setLanguage(shift);
}


sub thesaurusNew {
  my $obj = {
	     thesaurus => {},
	     inverses => default_inversions(),
	     descriptions => default_descriptions(),
	     externals => default_norelations(),
	     languages => {},
	    };

  # bless and return it! Amen!
  return bless($obj);
}

sub storeOn {
  store(@_);
}

sub thesaurusRetrieve {
  my $file = shift;
  my $obj = retrieve($file);
  return $obj;
}

sub tr_url {
  my $t = shift;
  $t =~ s/\s/+/g;
  return $t;
}

sub getHTMLTop {
  my $self = shift;
  my $script = shift || $ENV{SCRIPT_NAME};
  my $t = "<ul>";
  $t.=join("\n",
	   map { "<li><a href=\"$script?t=" .tr_url($_). "\">$_</a></li>" }
	   @{$self->{thesaurus}->{$self->{name}}->{NT}});
  $t .= "</ul>";
  return $t;
}

sub thesaurusLoad {
  my $file = shift;
  my %thesaurus;
  my $invs = default_inversions();
  my $descs = default_descriptions();
  my $norels = default_norelations();
  my $name = "_top_";
  my $languages = {};

  # Open the thesaurus file to charge
  open ISO, $file or die ($lang->get("cantopen"));

  # While we have commands or comments or empty lines, continue...
  while(($_ = <ISO>)=~/(^(%|#))|(^\s*$)/) {
    chomp;
    if (/^%\s*inv(erse)?\s+(\S+)\s+(\S+)/) {

      # Treat the inv*erse command
      $invs->{uc($2)}=uc($3);
      $invs->{uc($3)}=uc($2);

    } elsif (/^%\s*desc(ription)?\[(\S+)\]\s+(\S+)\s+/) {

      # Treat the desc*cription [lang] command
      $descs->{uc($3)." ".uc($2)} = $';

    } elsif (/^%\s*desc(ription)?\s+(\S+)\s+/) {

      # Treat the desc*cription command
      $descs->{uc($2)} = $';


    } elsif (/^%\s*ext(ernals?)?\s+/) {

      # Treat the ext*ernals command
      chomp(my $classes = uc($'));
      for (split /\s+/, $classes) {
	$norels->{$_} = 1;
      }

    } elsif (/^%\s*lang(uages?)?\s+/) {
      # Treat the lang*uages command
      chomp(my $classes = uc($'));
      for (split /\s+/, $classes) {
	$languages->{$_} = 1;
      }

    } elsif (/^%\s*top\s+(.*)$/) {
      $name = $1;
    }
  }

  # Redefine the record separator
  my $old_sep = $/;
  $/ = "\n\n";

  # The last line wasn't a comment, a command or an empty line, so use it!
  $_ .= <ISO>;

  # While there are definitions...
  do {
    # define local variables
    my ($class,$term,$Term);

    # The first line contains the term to be defined
    /(.*)\n/;
    $Term = $1;
    $term = lc($Term);

    $thesaurus{$term}{_NAME_} = $Term;

    # The remaining are relations
    $_ = $';

    # Let's see if the term is commented...
    unless ($term =~ /^#/) {
      # OK! The term is *not* commented...
      # For each definition line...
      while (/(([a-zA-Z]+)|#|)\s+(.*)\n/g) {
	# Is it commented?
	unless ($1 eq "#") {
	  # it seems not... set the relation class
	  $class = uc($1) || $class;
	  # See if $class has a description
	  $descs->{$class}=ucfirst(lc($class))  unless(defined($descs->{$class}));
	  # divide the relation terms by , if not language or extern
	  if (defined($norels->{$class}) || defined($languages->{$class})) {
	    $thesaurus{$term}{$class} = $3;
	  } else {
	    push(@{$thesaurus{$term}{$class}}, map {lc} split(/\s*,\s*/, $3));
	  }
	}
      }
    }
  } while(<ISO>);

  # Close the ISO thesaurus file
  close ISO;

  # revert to the old record separator. Not needed, but beautifer.
  $/ = $old_sep;

  # Define the Thesaurus object
  my $obj = {
	     thesaurus => \%thesaurus,
	     inverses => $invs,
	     descriptions => $descs,
	     externals => $norels,
	     languages => $languages,
	     name => $name,
	    };

  # bless and return it! Amen!
  return complete(bless($obj));
}

sub describe {
  my ($obj,$class,$desc) = @_;
  $obj->{descriptions}->{$class}=$desc;
}

sub addInverse {
  my ($obj,$a,$b) = @_;
  $obj->{descriptions}->{$a}="..." unless(defined($obj->{descriptions}->{$a}));
  $obj->{descriptions}->{$b}="..." unless(defined($obj->{descriptions}->{$b}));

  for (keys %{$obj->{inverses}}) {
    delete($obj->{inverses}->{$_}) if (($obj->{inverses}->{$_} eq $a) ||
				       ($obj->{inverses}->{$_} eq $b));
  }
  $obj->{inverses}->{$a}=$b;
  $obj->{inverses}->{$b}=$a;
}

sub save {
  my $obj = shift;
  my $file = shift;
  my ($term,$class);

  my %thesaurus = %{$obj->{thesaurus}};
  my %inverses = %{$obj->{inverses}};
  my %descs = %{$obj->{descriptions}};

  my $t = "";

  # Save the externals commands
  #
  $t.= "\%externals " . join(" ",keys %{$obj->{externals}});
  $t.="\n\n";

  # Save the languages commands
  #
  $t.= "\%languages " . join(" ",keys %{$obj->{languages}});
  $t.="\n\n";

  # Save the inverses commands
  #
  for $term (keys %inverses) {
    $t.= "\%inverse $term $inverses{$term}\n";
  }
  $t.="\n\n";

  # Save the descriptions commands
  #
  for $term (keys %descs) {
    if ( $term =~ /^(\w+)\s+(\w+)$/ ) {
      $t.= "\%description[$2] $1 $descs{$term}\n";
    } else {
      $t.= "\%description $term $descs{$term}\n";
    }
  }
  $t.="\n\n";

  # Save the thesaurus
  #
  for $term (keys %thesaurus) {
    $t.= "\n$thesaurus{$term}{_NAME_}\n";
    for $class ( keys %{$thesaurus{$term}} ) {
      next if $class eq "_NAME_";
      $t.= "$class\t" . join(", ", @{$thesaurus{$term}->{$class}}) . "\n";
    }
  }

  open F, ">$file" or return 0;
  print F $t;
  close F;
  return 1;
}

sub navigate {
  # The first element is the object reference
  my $obj = shift;
  # This is the script name
  my $script = $ENV{SCRIPT_NAME};

  # Get the configuration hash
  my $conf = {};
  if (ref($_[0])) {
    $conf = shift;
  }

  my $expander = $conf->{expand} || [];
  my $language = $conf->{lang} || undef;
  my %param = @_;
  my $term = lc($param{t}) || lc($obj->{name}) || '_top_';
  $term =~ s/\+/ /g;
  my (@terms,$html);

  # If we don't have the term, return only the title
  return "<h2>$term</h2>" unless (defined($obj->{thesaurus}{$term}));

  # Make the page title
  $html = "<h2>".$obj->translateTerm($term,$language)."</h2>";

  # Get the external relations
  my %norel = %{$obj->{externals}};

  # Now print the relations
  my $rel;
  for $rel (keys %{$obj->{thesaurus}{$term}}) {
    # next iteraction if the relation is the _NAME_
    next if ($rel eq "_NAME_");

    # This block jumps if it is an expansion relation
    next if grep {$_ eq uc($rel)} @{$expander};

    # The externs exceptions...
    if (exists($norel{$rel})) {
      # It's an external, so, blockquote it!!
      $html.= "<blockquote>$obj->{thesaurus}{$term}{$rel}</blockquote><br>";
    } elsif (exists($obj->{languages}{$rel})) {
      ## This empty block is used for languages translations
    } else {
      ## OK! It's a simple relation

      # There is a translation for the *relation* description?
      if ($language && exists($obj->{descriptions}{"$rel $language"})) {
	$html.= "<b>".$obj->{descriptions}{"$rel $language"}."</b>";
      } else {
	# And, there is a description?
	if (exists ($obj->{description}{$rel}) && $obj->{description}{$rel} eq "...") {
	  $html .= "<b>$rel</b> ";
	} else {
	  $html .= "<b>$obj->{descriptions}{$rel}:</b> ";
	}
      }

      # Now, write each term with a thesaurus link
      $html.= join(", ", map {
	my $term = $_;
	my $link = $term;
	$link =~ s/\s/+/g;
	$term = $obj->translateTerm($term, $language);
	"<a href=\"$script?t=$link\">$term</a>"
      } @{$obj->{thesaurus}{$term}{$rel}});

      $html.= "<br>";
    }
  }

  $html.= "<br>";

  # Now, treat the expansion relations
  for $rel (@{$expander}) {
    $rel = uc($rel);
    if (exists($obj->{thesaurus}{$term}{$rel})) {
      @terms =  @{$obj->{thesaurus}{$term}{$rel}};
      $html.= "<ul><li>" .
	join("</li><li>", map {thesaurusGetHTMLTerm($_, $obj, $script, $language)} @terms) .
	  "</li></ul>";
    }
  }
  return $html;
}

sub toTex{
  my $self = shift;
  $_corres = shift;
  my $mydt = shift;
  my $a;
 
  my %descs = %{$self->{descriptions}};

  my $procgr= sub {
      my $r=""; my $a;
      my $ki =  $_corres->{$class}->[0] || 
                  (defined $descs{$class} 
                   ? "\\\\\\emph{$descs{$class}} -- " 
                   : "\\\\\\emph{".ucfirst(lc($class))."} -- " 
                  );
      my $kf = $_corres->{$class}->[1] || "\n";
      for $a (@terms){
        $r .= $ki.$a.$kf;};
      $r
      };
 
"\\begin{description}\n".
 $self->full_dt(
#          '-default'  => $proc1,
           '-default'  => $procgr,
          '_NAME_' => sub{"\n".'\item['.$term."]~\n"},
          (%$mydt)
        ).
"\\end{description}\n";
}

sub dumpHTML {
  my $obj = shift;
  my %thesaurus = %{$obj->{thesaurus}};
  my $t = "";
  for (keys %thesaurus) {
    $t.=thesaurusGetHTMLTerm($_,$obj);
  }
  return $t;
}


sub relations {
  my ($self,$term) = @_;
  $term = lc($term);
  return sort keys %{$self->{thesaurus}->{$term}}
}

# Given a term, return it's information (second level for navigate)
sub thesaurusGetHTMLTerm {
  my ($term,$obj,$script,$language) = @_;

  # Put thesaurus and descriptions on handy variables
  my %thesaurus = %{$obj->{thesaurus}};
  my %descs = %{$obj->{descriptions}};

  # Set the term to lower case
  $term = lc($term);

  # Check if the term exists in the thesaurus
  if (exists($thesaurus{$term})) {
    my ($c,$t,$tterm);
    my $link = $term;

    $link =~ s/\s/+/g;
    $tterm = $obj->translateTerm($term,$language);
    $t = "<b><a href=\"$script?t=$link\">$tterm</a></b><br><small><ul>\n";

    for $c (sort keys %{$thesaurus{$term}}) {
      $c = uc($c);
      # jump if it is the name relation :)
      next if ($c eq "_NAME_");

      if (exists($obj->{externals}{$c})) {
	# put an external relation
	$t.= "<blockquote>$thesaurus{$term}{$c}</blockquote><br>";
      } elsif (exists($obj->{languages}{$c})) {
	# Jump the language relations
      } else {
	if (defined($language) && exists($descs{"$c $language"})) {
	  $t.= "<b>".$descs{"$c $language"}.":</b> ";
	} else {
	  if ($descs{$c} eq "...") {
	    $t.="<b>$c:</b> ";
	  } else {
	    $t.= "<b>$descs{$c}:</b> ";
	  }
	}
	my @termos = @{$thesaurus{$term}{$c}};
	if (defined($script)) {
	  @termos = map {my $link = $_;
			 $_ = $obj->translateTerm($_,$language);
			 $link =~s/\s/+/g;
			 "<a href=\"$script?t=$link\">$_</a>"} @termos;
	}
	$t.= join(", ", @termos) . "<br>\n";
      }
    }
    $t.= "</ul></small>\n";
    return $t;
  }
  else {
    return $lang->str("[tnothere]\n");
  }
}

sub complete {
  my $obj = shift;
  my %thesaurus = %{$obj->{thesaurus}};
  my %inverses = %{$obj->{inverses}};
  my ($termo,$classe);

  # para cada termo
  for $termo (keys %thesaurus) {
    # e para cada classe,
    for $classe (keys %{$thesaurus{$termo}}) {
      # se tiver inverso,
      if (defined($inverses{$classe})) {
	# completar cada um dos termos relacionados
	for (@{$thesaurus{$termo}{$classe}}) {
	  %thesaurus = completa(lc($_),$inverses{$classe},lc($termo),%thesaurus);
	}
      }
    }
  }

  $obj -> {thesaurus} = \%thesaurus;
  return $obj;
}

sub completa {
  my ($palavra,$classe,$termo,%thesaurus) = @_;

  # Ver se existe a palavra e a classe no thesaurus
  if (defined($thesaurus{$palavra}) && defined($thesaurus{$palavra}{$classe})) {
    # se existe, o array palavras fica com os termos (para ver se ja' existe)
    my @palavras = @{$thesaurus{$palavra}{$classe}};
    # ver se ja' existe
    for (@palavras) {
      return %thesaurus if (lc eq $termo);
    }
    # nao existe: aumentar
    push @{$thesaurus{$palavra}{$classe}}, $termo;
  } else {
    # nao existe: aumentar
    $thesaurus{$palavra}{_NAME_} = ucfirst($palavra) unless defined($thesaurus{$palavra}{_NAME_});
    push @{$thesaurus{$palavra}{$classe}}, $termo;
  }
  return %thesaurus;
}

sub addTerm {
  my $obj = shift;
  my $Term = shift;
  my $term = lc($Term);

  $obj->{thesaurus}->{$term}->{_NAME_} = $Term;
}

sub addRelation {
  my $obj = shift;
  my $term = lc(shift);
  my $rel = uc(shift);
  my @terms = @_;
  $obj->{descriptions}->{$rel}="..." unless(defined($obj->{descriptions}->{$rel}));
  push @{$obj->{thesaurus}->{$term}->{$rel}}, @terms;
}

sub deleteTerm {
  my $obj = shift;
  my $term = lc(shift);
  my ($t,$c);

  delete($obj->{thesaurus}->{$term});
  foreach $t (keys %{$obj->{thesaurus}}) {
    foreach $c (keys %{$obj->{thesaurus}->{$t}}) {
      my @a;
      foreach (@{$obj->{thesaurus}->{$t}->{$c}}) {
	push @a,$_ unless(lc($_) eq $term);
      }
      $obj->{thesaurus}->{$t}->{$c}=\@a;
    }
  }
}

#  sub edit {
#    my $obj = shift;
#    my $file = shift;
#    my %param = @_;
#    my $html = "";
#    if (%param) {
#      if ($param{comando} eq "introduzir") {
#        if (defined($param{termo})) {
#  	$obj -> addTerm($param{termo});
#  	for (keys %param) {
#  	  if (/terms(\d)/) {
#  	    if ($param{$_}) {
#  	      $obj->addRelation($param{termo},$param{"rel$1"},
#  					 split(/\s*,\s*/,$param{$_}));
#  	    }
#  	  }
#  	}
#  	$obj->storeOn($file);
#  	$html.=$lang->str("<center><table cellpadding=\"10\" border=\"1\"><tr><td><h2>[saved]</h2></td></tr></table></center>");
#        } else {
#  	$html.= $lang->str("<center><h2>[insert]</h2><table cellpadding=\"10\" border=\"1\"><tr><td>");
#  	my $ops = join "\n",
#  	  map {"<option value=\"$_\">$_"} (keys %{$obj->{descriptions}});
#  	$html.= "<FORM METHOD=\"POST\"  ENCTYPE=\"application/x-www-form-urlencoded\">";
#  	$html.= "<center><table border=\"0\">";

#  	$html.= $lang->str("<tr><td valign=\"top\"><b>[term]</b></td>");
#  	$html.= "    <td><input name=\"termo\"></td></tr>";

#  	$html.= $lang->str("<tr><td colspan=\"2\"><b>[rels]</b></td>");

#  	$html.= "<tr><td align=\"right\"><select name=\"rel1\">$ops</select></td>";
#  	$html.= "    <td><input name=\"terms1\" size=\"40\"></td></tr>";
#  	$html.= "<tr><td align=\"right\"><select name=\"rel2\">$ops</select></td>";
#  	$html.= "    <td><input name=\"terms2\" size=\"40\"></td></tr>";
#  	$html.= "<tr><td align=\"right\"><select name=\"rel3\">$ops</select></td>";
#  	$html.= "    <td><input name=\"terms3\" size=\"40\"></td></tr>";
#  	$html.= "<tr><td align=\"right\"><select name=\"rel4\">$ops</select></td>";
#  	$html.= "    <td><input name=\"terms4\" size=\"40\"></td></tr>";

#  	$html.= "<tr><td><input name=\"rel5\" size=\"8\"></td>";
#  	$html.= "    <td><input name=\"terms5\" size=\"40\"></td></tr>";
#  	$html.= "<tr><td><input name=\"rel6\" size=\"8\"></td>";
#  	$html.= "    <td><input name=\"terms6\" size=\"40\"></td></tr>";

#  	$html.= "<tr><td colspan=\"2\"><hr></td></tr>";
#  	$html.= "<tr><td colspan=\"2\" align=\"right\">";
#  	$html.= "<input type=\"hidden\" name=\"comando\" value=\"introduzir\">";
#  	$html.= $lang->str("<input type=\"submit\" value=\" [ok] \"></td></tr>");

#  	$html.= "</table></center>";
#  	$html.= "</FORM>";
#  	$html.= "</td></tr></table></center>";
#        }
#      } elsif ($param{comando} eq "apagar") {
#        if (defined($param{termo})) {
#  	$obj -> deleteTerm($param{termo});
#  	$obj -> storeOn($file);
#  	$html.=$lang->str("<center><table cellpadding=\"10\" border=\"1\"><tr><td><h2>[deleted]</h2></td></tr></table></center>");
#        } else {
#  	$html.= $lang->str("<center><h2>[delete]</h2><table cellpadding=\"10\" border=\"1\"><tr><td>");
#  	$html.= "<FORM METHOD=\"POST\"  ENCTYPE=\"application/x-www-form-urlencoded\">";
#  	$html.= "<center><table border=\"0\">";

#  	$html.= $lang->str("<tr><td valign=\"top\"><b>[termtodel]&nbsp;</b></td>");
#  	$html.= "    <td><input name=\"termo\"></td></tr>";

#  	$html.= "<tr><td colspan=\"2\"><hr></td></tr>";
#  	$html.= "<tr><td colspan=\"2\" align=\"right\">";
#  	$html.= "<input type=\"hidden\" name=\"comando\" value=\"apagar\">";
#  	$html.= $lang->str("<input type=\"submit\" value=\" [ok] \"></td></tr>");

#  	$html.= "</table></center>";
#  	$html.= "</FORM>";
#  	$html.= "</td></tr></table></center>";
#        }
#      } elsif ($param{comando} eq "descrever") {
#        if (defined($param{rel})) {
#  	$obj->describe($param{rel},$param{desc});
#  	$obj->storeOn($file);
#  	$html.="<center><table cellpadding=\"10\" border=\"1\"><tr><td><h2>";
#  	$html.=$lang->str("[saved]</h2></td></tr></table></center>");
#        } else {
#  	$html.= $lang->str("<center><h2>[descrel]</h2><table cellpadding=\"10\" border=\"1\"><tr><td>");
#  	$html.= "<FORM METHOD=\"POST\"  ENCTYPE=\"application/x-www-form-urlencoded\">";
#  	$html.= "<center><table border=\"0\">";

#  	$html.= $lang->str("<tr><td valign=\"top\"><b>[rel]:</b></td>");
#  	$html.= "    <td><input name=\"rel\"></td></tr>";
#  	$html.= $lang->str("<tr><td valign=\"top\"><b>[desc]:&nbsp;</b></td>");
#  	$html.= "    <td><input name=\"desc\"></td></tr>";

#  	$html.= "<tr><td colspan=\"2\"><hr></td></tr>";
#  	$html.= "<tr><td colspan=\"2\" align=\"right\">";
#  	$html.= "<input type=\"hidden\" name=\"comando\" value=\"descrever\">";
#  	$html.= $lang->str("<input type=\"submit\" value=\" [ok] \"></td></tr>");

#  	$html.= "</table></center>";
#  	$html.= "</FORM>";
#  	$html.= "</td></tr></table></center>";

#        }
#      } elsif ($param{comando} eq "inversos") {
#        if (defined($param{a})) {
#  	$obj->addInverse($param{a},$param{b});
#  	$obj->storeOn($file);
#  	$html.="<center><table cellpadding=\"10\" border=\"1\"><tr><td><h2>";
#  	$html.=$lang->str("[saved]</h2></td></tr></table></center>");
#        } else {
#  	$html.=$lang->str("<center><h2>[descinv]</h2><table cellpadding=\"10\" border=\"1\"><tr><td>");
#  	$html.= "<FORM METHOD=\"POST\"  ENCTYPE=\"application/x-www-form-urlencoded\">";
#  	$html.= "<center><table border=\"0\">";

#  	$html.= $lang->str("<tr><td valign=\"top\"><b>[rel] A</b></td>");
#  	$html.= "    <td><input name=\"a\"></td></tr>";
#  	$html.= $lang->str("<tr><td valign=\"top\"><b>[rel] B</b></td>");
#  	$html.= "    <td><input name=\"b\"></td></tr>";

#  	$html.= "<tr><td colspan=\"2\"><hr></td></tr>";
#  	$html.= "<tr><td colspan=\"2\" align=\"right\">";
#  	$html.= "<input type=\"hidden\" name=\"comando\" value=\"inversos\">";
#  	$html.= $lang->str("<input type=\"submit\" value=\" [ok] \"></td></tr>");

#  	$html.= "</table></center>";
#  	$html.= "</FORM>";
#  	$html.= "</td></tr></table></center>";
#        }
#      } elsif ($param{comando} eq "completar") {
#        $obj -> complete();
#        $obj -> storeOn($file);
#        $html.="<center><table cellpadding=\"10\" border=\"1\"><tr><td><h2>";
#        $html.=$lang->str("[ok]</h2></td></tr></table></center>");
#      } elsif ($param{comando} eq "iso") {
#        if (defined($param{file})) {
#  	if ($obj -> save($param{file})) {
#  	  $html.="<center><table cellpadding=\"10\" border=\"1\"><tr><td><h2>";
#  	  $html.=$lang->str("[saved]</h2></td></tr></table></center>");
#  	} else {
#  	  $html.="<center><table cellpadding=\"10\" border=\"1\"><tr><td align=\"center\"><h2>";
#  	  $html.=$lang->str("[error]</h2>[cant]</td></tr></table></center>");
#  	}
#        } else {
#  	$html.= $lang->str("<center><h2>[saviso]</h2><table cellpadding=\"10\" border=\"1\"><tr><td>");
#  	$html.= "<FORM METHOD=\"POST\"  ENCTYPE=\"application/x-www-form-urlencoded\">";
#  	$html.= "<center><table border=\"0\">";

#  	$html.= $lang->str("<tr><td valign=\"top\"><b>[file]:</b></td>");
#  	$html.= "    <td><input name=\"file\"></td></tr>";

#  	$html.= "<tr><td colspan=\"2\"><hr></td></tr>";
#  	$html.= "<tr><td colspan=\"2\" align=\"right\">";
#  	$html.= "<input type=\"hidden\" name=\"comando\" value=\"iso\">";
#  	$html.= $lang->str("<input type=\"submit\" value=\" [ok] \"></td></tr>");

#  	$html.= "</table></center>";
#  	$html.= "</FORM>";
#  	$html.= "</td></tr></table></center>";
#        }
#      }
#    }

#    $html.= $lang->str("<center><h2>[index]</h2><table border=\"1\" cellpadding=\"10\"><tr><td><table>");

#    $html.= $lang->str("<tr><td><b>[insert]</b></td><td>");
#    $html.= "<FORM METHOD=\"POST\"  ENCTYPE=\"application/x-www-form-urlencoded\">";
#    $html.= "<input type=\"submit\" value=\" >>> \">";
#    $html.= "<input type=\"hidden\" name=\"comando\" value=\"introduzir\">";
#    $html.= "</FORM></td></tr>";

#    $html.= $lang->str("<tr><td><b>[delete]</b></td><td>");
#    $html.= "<FORM METHOD=\"POST\"  ENCTYPE=\"application/x-www-form-urlencoded\">";
#    $html.= "<input type=\"submit\" value=\" >>> \">";
#    $html.= "<input type=\"hidden\" name=\"comando\" value=\"apagar\">";
#    $html.= "</FORM></td></tr>";

#    $html.= $lang->str("<tr><td><b>[descrel]</b></td><td>");
#    $html.= "<FORM METHOD=\"POST\"  ENCTYPE=\"application/x-www-form-urlencoded\">";
#    $html.= "<input type=\"submit\" value=\" >>> \">";
#    $html.= "<input type=\"hidden\" name=\"comando\" value=\"descrever\">";
#    $html.= "</FORM></td></tr>";

#    $html.= $lang->str("<tr><td><b>[descinv]</b></td><td>");
#    $html.= "<FORM METHOD=\"POST\"  ENCTYPE=\"application/x-www-form-urlencoded\">";
#    $html.= "<input type=\"submit\" value=\" >>> \">";
#    $html.= "<input type=\"hidden\" name=\"comando\" value=\"inversos\">";
#    $html.= "</FORM></td></tr>";

#    $html.= $lang->str("<tr><td><b>[saviso]</b></td><td>");
#    $html.= "<FORM METHOD=\"POST\"  ENCTYPE=\"application/x-www-form-urlencoded\">";
#    $html.= "<input type=\"submit\" value=\" >>> \">";
#    $html.= "<input type=\"hidden\" name=\"comando\" value=\"iso\">";
#    $html.= "</FORM></td></tr>";

#    $html.= $lang->str("<tr><td><b>[autocomplete]</b></td><td>");
#    $html.= "<FORM METHOD=\"POST\"  ENCTYPE=\"application/x-www-form-urlencoded\">";
#    $html.= "<input type=\"submit\" value=\" >>> \">";
#    $html.= "<input type=\"hidden\" name=\"comando\" value=\"completar\">";
#    $html.= "</FORM></td></tr>";


#    $html.= "</table></td></tr></table></center>";
#    return $html;
#  }

use Data::Dumper;
sub dt {
  my $self = shift;
  my $t = lc(shift);
  my %handler = @_;
  my $c;
  my $r = "";
  $term = $t;
  if (defined( $handler{"_NAME_"})){
    $r .=  &{$handler{"_NAME_"}};
  }
  for $c (keys %{$self->{thesaurus}->{$t}}) {
    next if ($c eq "_NAME_");

    # Set environment variables to dt function
    #
    # Class...
    #
    $class = $c;
    #
    # List of terms...
    #
      if ($self->{externals}->{$class} ||
	  $self->{languages}->{$class}) {
        @terms = ( $self->{thesaurus}{$t}{$class} );
      } else {
        @terms = @{$self->{thesaurus}{$t}{$class}};
      }

    #
    # Current term...
    #
    $term = $t;

    if (defined($handler{$class})) {
    $r .=  &{$handler{$class}};
    } elsif (defined($handler{-default})) {
    $r .=  &{$handler{-default}};
    }
  }
  $r;
}

sub full_dt {
  my $self = shift;
  my %h = @_;
  my $t;
  my $r="";
  for $t (sort keys %{$self->{thesaurus}}) {
    $r .= $self->dt($t,%h);
  }
  $r
}

sub tc{  my %x= tc_aux(@_);
         keys %x;
}

sub tc_aux {
        my ($self,$term,@relat) = @_;
        my %r=($term => 1 );
        for ($self->terms($term,@relat)) {
             %r=(%r, $_ => 1,  tc_aux($self,$_,@relat)) unless $r{$_};;
        }
        %r;
}

1;
__END__

=head1 NAME

DigLib::Thesaurus - Perl extension for managing an ISO thesaurus

=head1 SYNOPSIS

  use DigLib::Thesaurus;

  $obj = thesaurusNew();
  $obj = thesaurusLoad('iso-file');
  $obj = thesaurusRetrieve('storable-file');

  $obj->save('iso-file');
  $obj->storeOn('storable-file');

  $obj->addTerm('term');
  $obj->addRelation('term','relation','term1',...,'termn');
  $obj->deleteTerm('term');

  $obj->describe('Relation','description');
  $obj->addInverse('Relation1','Relation2');

  $html = $obj->navigate(+{configuration},%parameters);

  $html = $obj->getHTMLTop();

  $obj->dt('termo', %handler);
  $obj->full_dt(%handler);

  $obj->complete();

  $obj->tc('termo', 'relation1', 'relation2');
  $obj->depth_first('term', 2, "NT", "UF")

=head1 DESCRIPTION

A Thesaurus is a classification structure. We can see it as a graph
where nodes are terms and the vertices are relations between terms.

This module provides transparent methods to maintain Thesaurus files.
The module uses a subset from ISO 2788 wich defines some standard
features to be found on thesaurus files. This ISO includes a set of
relations that can be seen as standard but, this program can use user
defined ones.  So, it can be used on ISO or not ISO thesaurus files.

=head2 File Structure

Thesaurus used with this module are standard ASCII documents. This
file can contain processing instructions, comments or term
definitions. The instructions area is used to define new relations and
mathematic properties between them.

We can see the file with this structure:

   ______________
  |              |
  |    HEADER    | --> Can contain, only, processing instructions,
  |______________|     comment or empty lines.
  |              |
  |  Def Term 1  | --> Each term definition should be separated
  |              |     from each other with an empty line.
  |  Def Term 2  |
  |              |
  |     .....    |
  |              |
  |  Def Term n  |
  |______________|

Comments can appear on any line. Meanwhile, the comment character
(B<#>) should be the first character on the line (with no spaces
before).  Comments line span to the end of the line (until the first
carriage return).

Processing instructions lines, like comments, should start with the
percent sign (B<%>). We describe these instructions later on this
document.

Terms definitions can't contain any empty line because they are used
to separate definitions from each other. On the first line of term
definition record should appear the defined term. Next lines defines
relations with other terms. The first characters should be an
abbreviation of the relation (on upper case) and spaces. Then, should
appear a comma separated list of terms.

There can be more than one line with the same relation. Thesaurus module will
concatenate the lists. If you want to continue a list on the next line you
can repeat the relation term of leave some spaces between the start of the line
and the terms list.

Here is an example:

  Animal
  NT cat, dog, cow
     fish, ant
  NT camel
  BT Life being

  cat
  BT Animal
  SN domestic animal to be kicked when
     anything bad occurs.

There can be defined a special term (C<_top_>). It should be
used when you want a top tree for thesaurus navigation. So,
we normally define the C<_top_> term with the more interesting
terms to be navigated.

The B<ISO> subset used are:

=over 4

=item B<TT> - Top Term

The broadest term we can define about the current term.

=item B<NT> - Narrower Term

Terms more specific than current term.

=item B<BT> - Broader Term

More generic terms than current term.

=item B<USE> - Synonym

Another chances when finding a Synonym.

=item B<UF> - Quasi-Synonym

Terms that are no synonyms of current term but can be used,
sometimes with that meaning.

=item B<RT> - Related Term

Related term that can't be inserted on any other category.

=item B<SN> - Scope Note

Text. Note of context of the current term. Use for definitions or
comments about the scope you are using that term.

=back

=head2 Processing Instructions

Processing instructions, as said before, are written on a line starting
with the percent sign. Current commands are:

=over 4

=item B<top>

When presenting a thesaurus, we need a term, to know where to start.
Normally, we want the thesaurus to have some kind of top level, where
to start navigating. This command specifies that term, the term that
should be used when no term is specified.

Example:

  %top Contents

  Contents
  NT Biography ...
  RT ...

=item B<inv>erse

This command defines the mathematic inverse of the relation. That
is, if you define C<inverse A B> and you know that C<foo> is
related by C<A> with C<bar>, then, C<bar> is related by C<B>
with C<foo>.

Example:

  %inv BT NT
  %inverse UF USE

=item B<desc>ription

This command defines a description for some relation class. These
descriptions are used when outputing thesaurus on HTML.

Example:

  %desc SN Note of Scope
  %description IOF Instance of

If you are constructing a multi-lingue thesaurus, you will want to translate
the relation class description. To do this, you should use the C<description>
command with the language in from of it:

  %desc[PT] SN Nota de Contexto
  %description[PT] IOF Instância de

=item B<ext>ernals

This defines classes that does not relate terms but, instead, relate a term
with some text (a scope note, an url, etc.). This can be used like this:

  %ext SN URL
  %externals SN URL

Note that you can specify more than one relation type per line.

=item B<lang>uages

This other command permits the construction of a multi-lingue thesaurus. TO
specify languages classifiers (like PT, EN, FR, and so on) you can use one
of these lines:

  %lang PT EN FR
  %languages PT EN FR

To describe (legend) the language names, you should use the B<description>
command, so, you could append:

  %description PT Portuguese
  %description EN English
  %description FR French

=back

=head2 I18N

Internationalization functions, C<languages> and C<setLanguage> should
be used before any other function or constructor. Note that when loading
a saved thesaurus, descriptions defined on that file will be not translated.
That's important!

  interfaceLanguages()

This function returns a list of languages that can be used on the current
Thesaurus version.

  interfaceSetLanguage( <lang-name> )

This function turns on the language specified. So, it is the first function
you should call when using this module. By default, it uses Portuguese. Future
version can change this, so you should call it any way.

=head1 API

This module uses a perl object oriented model programming, so you
must create an object with one of the C<thesaurusNew>, C<thesaurusLoad>
or C<thesaurusRetrieve> commands. Next commands should be called
using the B<OO> fashion.

=head1 Constructors

=head2 thesaurusNew

To create an empty thesaurus object. The returned newly created
object conains the inversion properties from the ISO classes and
some stub descriptions for the same classes.

=head2 thesaurusLoad

To use the C<thesaurusLoad> function, you must supply a file name.
This file name should correspond to the ISO ASCII file as defined
on earlier sections. It returns the object with the contents of
the file. If the file does not defined relations and descriptions
about the ISO classes, they are added.

=head2 thesaurusRetrieve

Everybody knows that text access and parsing of files is not
efficient. So, this module can save and load thesaurus from
Storable files. This funtion should receive a file name from
a file wich was saved using the C<storeOn> function.

=head1 Methods

=head2 save

This method dumps the object on an ISO ASCII file. Note that
the sequence C<thesaurusLoad>, C<save> is not the
identity function. Comments are removed and processing instructions
can be added. To use it, you should supply a file name.

Note: if the process fails, this method will return 0. Any
other method die when failing to save on a file.

=head2 storeOn

This method saves the thesaurus object in Storable format. You
should use it when you want to load with the C<thesaurusRetrieve>
function.

=head2 addTerm

You can add terms definitions using the perl API. This method adds
a term on the thesaurus. Note that if that term already exists, all
it's relations will be deleted.

=head2 addRelation

To add relations to a term, use this method. It can be called
again and again. Previous inserted relations will not be deleted.
This method can be used with a list of terms for the relation like:

  $obj->thesaurusAddRelation('Animal','NT','cat','dog','cow','camel');

=head2 deleteTerm

Use this method to remove all references of the term supplied. Note that
B<all> references will be deleted.

=head2 describe

You can use this method to describe some relation class. You can
use it to change the description of an existing class (like the ISO
ones) or to define a new class.

=head2 addInverse

This method should be used to describe the inversion property to
relation classes. Note that if there is some previous property
about any of the relations, it will de deleted. If any of the
relations does not exist, it will be added.

=head2 navigate

This function is a some what type of CGI included on a object
method. You must supply an associative array of CGI parameters. This
method prints an HTML thesaurus for Web Navigation.

The typical thesaurus navigation CGI is:

  #!/usr/bin/perl -w

  use CGI qw/:standard/;

  print header;
  for (param()) { $arg{$_} = param($_) }
  $thesaurus = thesaurusLoad("thesaurus_file");
  $thesaurus->navigate(%arg);

This method can receive, as first argument, a reference to an
associative array with some configuration variables like what
relations to be expanded and what language to be used by default.

So, in the last example we could write

  $thesaurus->navigate(+{expand=>['NT', 'USE'],
                         lang  =>'EN'})

meaning that the structure should show two levels of 'NT' and 'USE'
relations, and that it should use the english language.

=head2 complete

This function completes the thesaurus based on the invertibility
properties. This operation is only needed when adding terms and
relations by this API. Whenever the system loads a thesaurus ISO file,
it is completed.

=head2 dt and full_dt

The C<dt> method is used to produce something from some term information.
It should be passed as argument a term and an associative array with
anonymous subroutines that process each class. Example:

  $the->dt("frog", {NT => sub{ #Do nothing
                             },
                    -default => sub{ print "$class", join(",",@terms) }});


The full_dt method does not receive a term and calls the dt method for all
terms in the thesaurus.

=head2 C<depth_first> 

The C<depth_first> method is used to get the list of terms related
with C<$term> by relations C<@r> up to the level C<$lev>

  $the->depth_first($term ,$lev, @r)

  $the->depth_first("frog", 2, "NT","UF")

=head2 C<tc> transitive closure

The C<tc> method is used to eval the transitive closure of the relations
C<@r> starting from a term C<$term>

  $the->tc($term , @r)

  $the->tc("frog", "NT","UF")

=head2 C<terms>

The C<terms> method is used to get all the terms related by relations C<@r>
with C<$term>

  $the->terms($term , @r)

  $the->terms("frog", "NT","UF")

=head1 AUTHORS

Alberto Simões, <albie@alfarrabio.um.geira.pt>

José João Almeida, <jj@di.uminho.pt>

Sara Correia,  <sara.correia@portugalmail.com>

This module is included in the Natura project. You can visit it at
http://natura.di.uminho.pt.

=head1 SEE ALSO

The example thesaurus file (C<examples/thesaurus>), DigLib::MLang(3)
and perl(1) manpages.

=cut
