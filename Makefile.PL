use inc::Module::Install;

name     'Email-RDF';
all_from 'lib/Email/RDF.pm';
author   q{Dorian Taylor <dorian@cpan.org>};
license  'apache';

build_requires 'Test::More';

auto_install;

WriteAll;

