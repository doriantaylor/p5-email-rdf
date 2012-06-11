#!perl -T

use Test::More tests => 2;

BEGIN {
    use_ok( 'Email::RDF' ) || print "Bail out!\n";
    use_ok( 'Email::RDF::Nepomuk' ) || print "Bail out!\n";
}

diag( "Testing Email::RDF $Email::RDF::VERSION, Perl $], $^X" );
