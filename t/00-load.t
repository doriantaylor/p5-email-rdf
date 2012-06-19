#!perl -T

use Test::More tests => 3;

BEGIN {
    use_ok( 'Email::RDF' )               || print "Bail out!\n";
    use_ok( 'Email::RDF::Driver' )       || print "Bail out!\n";
    use_ok( 'Email::RDF::Driver::SIOC' ) || print "Bail out!\n";
}

diag( "Testing Email::RDF $Email::RDF::VERSION, Perl $], $^X" );
