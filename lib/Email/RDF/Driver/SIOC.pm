package Email::RDF::Driver::SIOC;

use strict;
use warnings FATAL => 'all';

use Moose;
use namespace::autoclean;

extends 'Email::RDF::Driver';

use RDF::Trine;

# bootstrap
my $NS = RDF::Trine::NamespaceMap->new({
    rdf   => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    rdfs  => 'http://www.w3.org/2000/01/rdf-schema#',
    xsd   => 'http://www.w3.org/2001/XMLSchema#',
    owl   => 'http://www.w3.org/2002/07/owl#',
    dct   => 'http://purl.org/dc/terms/',
    sioc  => 'http://rdfs.org/sioc/ns#',
    sioct => 'http://rdfs.org/sioc/types#',
    foaf  => 'http://xmlns.com/foaf/0.1/',
    bibo  => 'http://purl.org/ontology/bibo/',
});

my %HTML = (
    head       => { profile  => $NS->dct('references') },
    script     => { src      => $NS->dct('requires') },
    a          => { href     => $NS->dct('references') },
    area       => { href     => $NS->dct('references') },
    link       => { href     => $NS->dct('references') },
    form       => { action   => $NS->dct('references') },
    blockquote => { cite     => $NS->dct('source') },
    q          => { cite     => $NS->dct('source') },
    ins        => { cite     => $NS->dct('source') },
    del        => { cite     => $NS->dct('source') },
    frame      => { src      => $NS->dct('hasPart'),
                    longdesc => $NS->dct('references') },
    iframe     => { src      => $NS->dct('hasPart'),
                    longdesc => $NS->dct('references') },
    img        => { src      => $NS->dct('hasPart'),
                    longdesc => $NS->dct('references'),
                    usemap   => $NS->dct('requires') },
    object     => { data     => $NS->dct('hasPart'),
                    classid  => $NS->dct('requires'),
                    codebase => $NS->dct('requires'),
                    usemap   => $NS->dct('requires') },
    input      => { src      => $NS->dct('hasPart'),
                    usemap   => $NS->dct('requires') },
);

=head1 NAME

Email::RDF::Driver::SIOC - SIOC driver for Email::RDF

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

    # that's it
    my $emrdf = Email::RDF->new(driver => 'SIOC');

=head1 METHODS

=head2 create_thread

=cut

# XXX fix this

sub namespaces {
    return $NS;
}

sub html_map {
    return \%HTML;
}

sub link_xpath {
    join('|', map { "//html:$_" } keys %HTML);
}

sub create_thread {
    my ($self, $uri) = @_;
    my $s = RDF::Trine::Node::Resource->new("$uri");

    RDF::Trine::Statement->new($s, $NS->rdf('type'), $NS->sioc('Thread'));
}

=head2 add_to_thread

=cut

sub add_to_thread {
    my ($self, $thread, $message) = @_;
    my $s = RDF::Trine::Node::Resource->new("$thread");
    my $o = RDF::Trine::Node::Resource->new("$message");
    return (
        RDF::Trine::Statement->new($s, $NS->sioc('container_of'), $o),
        RDF::Trine::Statement->new($o, $NS->sioc('has_container'), $s));
}

=head2 create_message

=cut

sub create_message {
    my ($self, $message) = @_;
    my $s = RDF::Trine::Node::Resource->new("$message");
    RDF::Trine::Statement->new($s, $NS->rdf('type'), $NS->sioct('MailMessage'));
}

=head2 set_parent

=cut

sub set_parent {
    my ($self, $message, $parent) = @_;
    my $s = RDF::Trine::Node::Resource->new("$message");
    my $o = RDF::Trine::Node::Resource->new("$parent");

    #warn $parent;

    return (RDF::Trine::Statement->new($s, $NS->sioc('reply_of'), $o),
            RDF::Trine::Statement->new($o, $NS->sioc('has_reply'), $s));
}

=head2 map_headers

=cut

sub _mailto {
    my $hdr = shift;
    # XXX replace with Email::Address
    $hdr =~ s/^.*?<([^>]*)>.*/$1/;
    my $mt = URI->new('mailto:' . $hdr)->canonical;
    RDF::Trine::Node::Resource->new("$mt");
}

sub _literal {
    RDF::Trine::Node::Literal->new(shift);
}

sub map_headers {
    my ($self, $message, $headers) = @_;
    my %x = (
        'To'      => [$NS->sioc('addressed_to'), \&_mailto],
        'Cc'      => [$NS->sioc('addressed_to'), \&_mailto],
        'Bcc'     => [$NS->sioc('addressed_to'), \&_mailto],
        'From'    => [$NS->dct('creator'),       \&_mailto],
        'Subject' => [$NS->dct('title'),        \&_literal],
    );

    my $s = RDF::Trine::Node::Resource->new("$message");

    my @statements;
    for my $k (keys %x) {
        my $val = $headers->header($k);
        if (defined $val) {
            my $o = $x{$k}[1]->($val);
            push @statements, RDF::Trine::Statement->new($s, $x{$k}[0], $o);
        }
    }
    @statements;
}

=head2 add_content

=cut

sub add_content {
    my ($self, $message, $content) = @_;
    my $s = RDF::Trine::Node::Resource->new("$message");
    my $o = RDF::Trine::Node::Literal->new($content);

    RDF::Trine::Statement->new($s, $NS->sioc('content'), $o);
}

=head2 add_reference

=cut

sub add_reference {
    my ($self, $message, $link) = @_;
    my $s = RDF::Trine::Node::Resource->new("$message");
    my $o = RDF::Trine::Node::Resource->new("$link");

    RDF::Trine::Statement->new($s, $NS->dct('references'), $o);
}

=head2 add_equivalent

=cut

sub add_equivalent {
    my ($self, $left, $right) = @_;
    my $s = RDF::Trine::Node::Resource->new("$left");
    my $o = RDF::Trine::Node::Resource->new("$right");

    return (RDF::Trine::Statement->new($s, $NS->owl('sameAs'), $o),
            RDF::Trine::Statement->new($o, $NS->owl('sameAs'), $s));
}

=head2 add_format

=cut

sub add_format {
    my ($self, $message, $target) = @_;
    my $s = RDF::Trine::Node::Resource->new("$message");
    my $o = RDF::Trine::Node::Resource->new("$target");

    RDF::Trine::Statement->new($s, $NS->dct('hasFormat'), $o);
}

=head2 add_type

=cut

my %TYPES = (
    'text/html'             => $NS->bibo('Webpage'),
    'application/xhtml+xml' => $NS->bibo('Webpage'),
    image                   => $NS->foaf('Image'),
);

sub add_type {
    my ($self, $uri, $type) = @_;
    my $s = RDF::Trine::Node::Resource->new("$uri");

    $type =~ s/^\s*(.*?)(;.*)?$/\L$1/;
    my $o = RDF::Trine::Node::Literal->new($type);

    my ($major, $minor) = split m!/+!, $type;
    my $rdftype = $TYPES{$type} || $TYPES{$major} || $NS->foaf('Document');

    return (RDF::Trine::Statement->new($s, $NS->rdf('type'), $rdftype),
            RDF::Trine::Statement->new($s, $NS->dct('format'), $o));
}

=head2 add_html_link

=cut

sub add_html_links {
    my ($self, $message, $node, $base) = @_;

    my @statements;

    if (my $which = $HTML{$node->localName}) {
        $base = URI->new($base) if defined $base;

        my $s = RDF::Trine::Node::Resource->new("$message");

        for my $attr (keys %{$which}) {
            next unless $node->hasAttribute($attr);

            my $uri = $node->getAttribute($attr);
            $uri = $base ? URI->new_abs($uri, $base) : URI->new($uri);

            # don't include relative URIs
            if ($uri->scheme) {
                my $p = $which->{$attr};
                my $o = RDF::Trine::Node::Resource->new("$uri");
                push @statements, RDF::Trine::Statement->new($s, $p, $o);
            }
        }
    }

    @statements;
}

=head1 AUTHOR

Dorian Taylor, C<< <dorian at cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-email-rdf at
rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Email-RDF>.  I will
be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Email::RDF::Driver::SIOC

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Email-RDF>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Email-RDF>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Email-RDF>

=item * Search CPAN

L<http://search.cpan.org/dist/Email-RDF/>

=back

=head1 LICENSE AND COPYRIGHT

Copyright 2012 Dorian Taylor.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this file except in compliance with the License.  You may
obtain a copy of the License at
L<http://www.apache.org/licenses/LICENSE-2.0>

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied.  See the License for the specific language governing
permissions and limitations under the License.


=cut

1; # End of Email::RDF::Nepomuk
