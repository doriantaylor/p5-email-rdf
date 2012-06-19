package Email::RDF;

use 5.008;
use strict;
use warnings FATAL => 'all';

use Moose;
use namespace::autoclean;

use Scalar::Util  ();
use RDF::Trine    ();
use Email::Thread ();
use OSSP::uuid    ();
use URI::mid      ();
use URI::di       ();
# aw yiss
use URI::Find::Schemeless ();

# lying headers!
use Encode        ();
use Encode::Guess ();
# html crap
use HTML::Tidy    ();
use XML::LibXML   ();

=head1 NAME

Email::RDF - Produce RDF triples from email messages

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

    # First: get some Email::MIME objects from somewhere.

    package My::Email::Folder;

    use base qw(Email::Folder);
    use Email::MIME;

    # Read the docs on Email::Folder to see why I'm doing this.

    sub bless_message {
        Email::MIME->new($_[1]);
    }

    package main;

    use Email::RDF;

    my $folder = My::Email::Folder->new('my.mbox');

    # Equivalent to Email::RDF->new;
    my $emrdf  = Email::RDF->new(
        driver => 'SIOC',
        model  => RDF::Trine::Model->temporary_model,
    );

    # This might take up some RAM.
    # Also note that every time you do this, the threading will be
    # recomputed, so best to do it in bulk.

    $emrdf->add($folder->messages);

    # Deal with attachments. $id is a di: URI (see URI::di). Needless
    # to say, the content of the attachment will match the
    # cryptographic digest in the identifier.

    for my $id ($emrdf->attachments) {

         # This object collapses all bitwise-identical attachments
         # together with a list of any filenames they were ascribed.

         my ($dataref, $type, @names) = $emrdf->attachment($id);

         # save to disk, database, whatever
         do_something_with($dataref, $names[0] || $id->hexdigest);
    }

    # Now you can dump the triples.
    print $emrdf->serialize('turtle');

    # Or, assuming you didn't pass one into the constructor, you can
    # bulk-insert the results into another RDF::Trine::Model:
    $emrdf->copy_to($other_model);

=head1 DESCRIPTION

L<Email::RDF> will scan a list of L<Email::MIME> messages, thread them
according to JWZ's algorithm (see L<Email::Thread>), generate a set of
RDF triples specified by an L<Email::RDF::Driver> subclass, export all
attachments, and make a fantastic cocktail.

Well, four out of five ain't bad.

=head1 METHODS

=head2 new %OPTIONS

=over 4

=item driver

Over the years there have been a few attempts at representing email in
RDF. There was the original W3C note, Simile at MIT, some work by
Graham Klyne, and finally NEPOMUK and SIOC, which I have
implemented. There may potentially be others.

One of my selection criteria was that the vocabulary have a live,
dereferenceable namespace. The only one of the above that do are
NEPOMUK and SIOC.

Here are the attempts I could find, in chronological order:

=over 4

=item http://www.w3.org/2000/04/maillog2rdf/email

=item http://tools.ietf.org/html/draft-klyne-message-rfc822-xml-03

=item http://simile.mit.edu/wiki/Email_RDFizer

=item http://www.w3.org/WAI/ER/HTTP/WD-RFC822-in-RDF-20060502

=item http://sioc-project.org/ontology

=item http://www.semanticdesktop.org/ontologies/nmo/

=back

Also worth mentioning is SIOC's types module which includes a
sioct:MailMessage class which is a subclass of sioc:Post.

Actually you know what? I should just implement SIOC instead.

=item model

This should be an instance of L<RDF::Trine::Model>. It's only
necessary if you want this module to write directly to an existing RDF
graph. If you don't supply one, it will just use
L<RDF::Trine::Model/temporary_model>, from which you can serialize or
extract triples.

=back

=cut

# XXX should probably use types/method signatures but i don't know
# what's worse, coding crap like this or trying to deal with erroneous
# line numbers in tracebacks.
sub _really_is {
    my ($obj, $class) = shift;
    return unless ref $obj;
    return Scalar::Util::blessed($obj) ? $obj->isa($class) : ref $obj eq $class;
}

has model => (
    is      => 'ro',
    isa     => 'RDF::Trine::Model',
    lazy    => 1,
    default => sub { RDF::Trine::Model->temporary_model },
);

has driver => (
    is      => 'ro',
    isa     => 'Email::RDF::Driver',
    lazy    => 1,
    default => sub {
        require Email::RDF::Driver::SIOC;
        return  Email::RDF::Driver::SIOC->new },
);

has xpc => (
    is      => 'ro',
    isa     => 'XML::LibXML::XPathContext',
    lazy    => 1,
    default => sub {
        my $x = XML::LibXML::XPathContext->new;
        $x->registerNs(html => 'http://www.w3.org/1999/xhtml');
        $x },
);

has _attach => (
    is => 'ro',
    lazy => 1,
    default => sub { {} },
);

sub link_nodes {
    my ($self, $doc) = @_;
    $self->xpc->findnodes($self->driver->link_xpath, $doc);
}

has _uuid => (
    is      => 'ro',
    isa     => 'OSSP::uuid',
    lazy    => 1,
    default => sub { OSSP::uuid->new },
);

sub BUILD {
    my $self = shift;

    # derp i forgot moose basically does this

    # if ($self->{model}) {
    #     # no need for defined, since 0 and "" are invalid.
    #     Carp::croak
    #           ("'model' parameter needs to be an RDF::Trine::Model instance")
    #               unless _really_is($self->{model}, 'RDF::Trine::Model');
    # }
    # else {
    #     $self->{model} = RDF::Trine::Model->temporary_model;
    # }

    # if (defined $self->{driver}) {
    #     # this can be a string, a fully-qualified class name or a
    #     # driver instance.
    # }
    # else {
    #     # default to SIOC
    #     require Email::RDF::Driver::SIOC;
    #     $self->{driver} = Email::RDF::Driver::SIOC->new;
    # }
}

=head2 add @MESSAGES

Add a list of L<Email::MIME> messages to the processor.

=cut

# each of these methods returns a list of statements which get added
# to the graph.

my %MULTIPART = (
    'multipart/mixed'       => \&_multipart_mixed,
    'multipart/related'     => \&_multipart_related,
    'multipart/alternative' => \&_multipart_alternative,
    'multipart/parallel'    => \&_multipart_parallel,
    'multipart/digest'      => \&_multipart_digest,
    'multipart/signed'      => \&_multipart_signed,
    'multipart/encrypted'   => \&_multipart_encrypted,
);

my %WHOLE = (
    'message/rfc822'        => \&_message_rfc822,
#    'text/html'             => \&_text_html,
#    'application/xhtml+xml' => \&_text_html,
#    'text/plain'            => \&_text_plain,
);

sub _dispatch_part {
    my ($self, $part, $mid, $hdr) = @_;
    my $ct = $part->content_type;
    # retrieve lower-case mime type sans parameters
    $ct =~ s/^\s*(.*?)(\s*;.*)?$/\L$1/;

    # lol wow
    my $sub = $ct =~ m!^multipart! ? $MULTIPART{$ct} || \&_multipart_mixed
        : $WHOLE{$ct} || \&_generic_attachment;

    # run the selected subroutine
    $self->$sub($part, $mid, $hdr);
}

# a message
sub _message_rfc822 {
    my ($self, $part, $parent) = @_;
    my @statements;

    my $mid    = URI->new('mid:' . $part->header('Message-ID'));
    my $driver = $self->driver;

    #push @statements, $driver->new_message($mid);
    #push @statements, $driver->do_headers($mid, $part);

    # we are an attachment
    #push @statements, $driver->attach($mid => $parent) if $parent;

    #push @statements, $self->_subparts($mid, $part->subparts);

    @statements;
}

# basic email with attachment
sub _multipart_mixed {
    my ($self, $part, $mid) = @_;

    # warn 'harro mixed';

    my @statements;

    # XXX I don't know of a good way to do this so I'm just going to
    # say that the first subpart contains the message content no
    # matter what it is.
    my $seen_content = 0;

    for my $subpart ($part->subparts) {
        if ($subpart->subparts) {
            # XXX this is actually kind of naive, since some subpart
            # might actually contain the content
            push @statements, $self->_dispatch_part($subpart, $mid);
        }
        else {
            my $disp = $subpart->header('Content-Disposition') || '';
            my $cid  = $subpart->header('Content-ID') || '';
            my $ct   = lc($subpart->content_type || '');

            if ($cid) {
                # absolutize the content-id
                my $c = URI::cid->parse($cid);
                my $m = $mid->clone;
                $cid  = $m->cid($c);
                warn $cid;
            }

            if ($disp =~ /^\s*attachment/i) {
            }
            elsif ($disp =~ /^\s*inline/i) {
            }
            else {
                # might still be a
            }

            if ($seen_content) {
                # this is an attachment, no matter what it is
                push @statements, $self->_dispatch_part($subpart, $mid);
            }
            else {
                # this might be the content if it's text or html
            }

            if (defined $disp and $disp ne '') {
            }
            else {
            }

            if ($ct =~ m!^text/plain!) {
            }
            # we need a little intelligence here

            # Suppose we have two subparts and they're both text/plain?
            # How do you tell which is the message content and which is
            # the attachment?

            # Answer: Content-Disposition header!
        }

        $seen_content++;
    }

    @statements;
}

# attachments which are used in one of the parts
sub _multipart_related {
    my ($self, $part, $mid) = @_;

    #warn 'harro rerated';

    my @statements;

    for my $subpart ($part->subparts) {
        # trace if multipart
        if ($subpart->subparts) {
        }
        else {
            # every subpart that isn't itself multipart is connected to the
            # parent by, e.g., dct:hasPart
        }
    }

    @statements;
}

sub _multipart_alternative {
    my ($self, $part, $mid) = @_;

    #warn 'harro arternative';

    my @statements;

    for my $subpart ($part->subparts) {
        # trace if the body is multipart (weird, but not impossible)
        if ($subpart->subparts) {
            push @statements, $self->_dispatch_part($subpart, $mid);
        }
        else {
            my $ct = lc($subpart->content_type || '');
            if ($ct =~ m!^text/plain!) {
                # if the body is text, process it as a literal as
                # sioc:content
                my $content = $self->_text_plain($subpart, $mid, 1);
                push @statements, $self->_text_references($content, $mid);
                push @statements, $self->driver->add_content($mid, $content);
            }
            elsif ($ct =~ m!^(?:text/html|application/xhtml+xml)!) {
                my $content = $self->_text_html($subpart, $mid, 1);
                if ($content) {
                    my $string  = $content->toString;
                    my $id      = URI::di->compute(\$string);
                    push @statements, $self->_html_references($content, $mid);
                    push @statements, $self->driver->add_format($mid, $id);
                }
            }
            else {
                my $id = $self->_process_representation($subpart);
                # if the body is something else, process it as dct:hasFormat
                push @statements, $self->driver->add_format($mid, $id);
            }
        }
    }
    @statements;
}

sub _multipart_parallel {
    # this should probably be handled as a collection of some kind
}

sub _multipart_digest {
    # do this later
}

sub _multipart_signed {
    # do this later
}

sub _multipart_encrypted {
    # do this later
}

# all the other multiparts get treated as multipart/mixed

# this is 
sub _process_representation {
    my ($self, $part, $mid) = @_;

}

sub _text_plain {
    my ($self, $part, $mid, $literal) = @_;
    # convert to utf-8
    #warn 'harro text';

    # XXX this might be problematic in conjunction with signing, since
    # we are re-encoding and trimming whitespace. Not sure at this
    # time how it ought to behave.

    my $body    = $part->body;
    my $decoder = eval { Encode::Guess::guess_encoding
          ($body, qw(ascii latin1 utf8 utf16 utf32)) };
    $body = $decoder->decode($body) if $decoder;

    # get rid of leading and trailing whitespace
    $body =~ s/^\s*(.*?)\s*$/$1/s;

    return $body if $literal;

    my $id = URI::di->compute(\$body);
}

sub _text_references {
    my ($self, $text, $mid) = @_;
    # subject is not necessarily a message-id, but rather the di: uri
    # of an attached text file

    my @links;
    my $finder = URI::Find::Schemeless->new
        (sub { push @links, URI->new(shift) });
    $finder->find(\$text);

    my @statements;
    for my $link (@links) {
        # absolutize (unlikely) content-ids found in text content
        if ($link->isa('URI::cid')) {
            my $m = $mid->clone;
            $link = $m->cid($link);
        }
        push @statements, $self->driver->add_reference($mid, $link);
    }

    @statements;
}

sub _text_html {
    my ($self, $part, $mid) = @_;

    # tidy
    # xhtml utf8
    #warn 'harro html';

    my $ti = HTML::Tidy->new({
        tidy_mark    => 0, wrap => 0, clean => 1, bare => 1, word_2000 => 0,
        output_xhtml => 1, output_encoding => 'utf8',
        add_xml_decl => 1, doctype => 'omit', numeric_entities => 1 });

    my $tidied = eval { $ti->clean($part->body) };
    if ($tidied) {
        #my $dom = eval { XML::LibXML->load_html
        #  ($part->body, { recover => 1, no_defdtd => 1, no_network => 1 }) };

        # what is this nonsense, microsoft?
        my $wtf = 'http://schemas.microsoft.com/2009/06/sip/state';
        $tidied =~ s/(xmlns:st\s*=\s*)['"]\s*(?:\x01|\&\#[x]?0*1;|)\s*['"]
                    /xmlns:st="$wtf"/gsx;

        # also wtf @ this
        $tidied =~ s!xmlns(:.*?)?
                     \s*=\s*['"]\s*http://www.w3.org/TR/REC-html40\s*['"]
                    !xmlns$1="http://www.w3.org/1999/xhtml"!gsx;

        # sigh
        $tidied =~ s/<![^[-]*(\[if.*?endif\])[^\]>-]*>/<!-- $1 -->/isg;

        #for my $msg ($ti->messages) {
        #    warn $msg->as_string;
        #}

        my $doc = eval { XML::LibXML->load_xml
              (string => $tidied, { recover => 0, no_network => 1 }) };
        if ($doc) {
            #warn $doc;
            return $doc;
        }
        else {
            warn $@;
            warn $tidied;
        }
    }
    else {
        warn "can't tidy :(";
    }

    return;
}

sub _html_references {
    my ($self, $doc, $mid) = @_;

    my ($base) = $self->xpc->findnodes
        ('/html:html/html:head/html:base[1]', $doc);
    if ($base) {
        $base = $base->getAttribute('href');
    }

    my @statements;
    for my $link ($self->link_nodes($doc)) {
        push @statements, $self->driver->add_html_links($mid, $link, $base);
    }
    @statements;
}

sub _generic_attachment {
    my ($self, $part, $mid) = @_;
    #warn 'harro generic: ' . $part->content_type;

    my @statements;

    my $body = $part->body;
    my $id   = URI::di->compute($body);
    my $ct   = $part->content_type;

    $ct =~ s/^\s*(.*?)(?:;.*)\s*$/\L$1/;

    push @statements, $self->driver->add_type($id, $ct);

    $self->_attach->{$id} ||= [\$body, $ct];

    @statements;
}

sub _process_mimepart {
    my ($self, $part, $mid) = @_;
    if (defined $mid) {
        # this is a subpart.

        # so ifi 
    }

    $mid ||= URI::mid->parse($part->header('Message-Id'));
    #warn $mid;

    my @statements;

    # check headers

    push @statements, $self->driver->map_headers($mid, $part->header_obj);
    push @statements, $self->_dispatch_part($part, $mid);

    if ($part->subparts) {
        # drill down to subparts
    }
    else {
        # save the body, default sha-256
        my $body = $part->body;
        my $id = URI::di->compute($body);
        my $ct = $part->content_type;
        $ct =~ s/^\s*(.*?)(?:;.*)\s*$/\L$1/;

        push @statements, $self->driver->add_type($id, $ct);

        #warn $id;
        $self->_attach->{$id} ||= [\$body, $ct];
    }

    # check mime parts
    #push @statements, $self->driver->attach(

    # message/rfc822
    # multipart/mixed *
    # multipart/related *
    # multipart/alternative *
    # multipart/parallel
    # multipart/digest
    # multipart/signed *
    # multipart/encrypted *

    # text/plain

    @statements;
}

sub _process {
    my ($self, $container, $thread, $parent) = @_;

    # first things first
    return $self->_process($container->child, $thread)
        if $container->messageid eq 'subject dummy';

    my $mid = URI->new('mid:' . $container->messageid);

    warn $container->child->subject
        if $container->messageid eq 'subject dummy'
            and $container->parent;

    my @statements = $self->driver->create_message($mid);

    # add this message to the thread
    push @statements, $self->driver->add_to_thread($thread, $mid);
    push @statements, $self->driver->set_parent($mid, $parent) if $parent;

    # process this message if it exists
    push @statements, $self->_process_mimepart($container->message)
        if $container->message;

    # now recurse to the other messages
    push @statements, $self->_process($container->child, $thread, $mid)
        if $container->child;
    push @statements, $self->_process($container->next,  $thread, $parent)
        if $container->next;

    @statements;
}

sub add {
    my ($self, @messages) = @_;

    # TODO: pre-scan attachments for message/rfc822 parts,
    # i.e. messages *as* attachments (and for that matter attached
    # messages that attach messages).

    my $threader = Email::Thread->new(@messages);
    $threader->thread;

    for my $root ($threader->rootset) {
        # create a thread object
        my $thread = $self->mint_uuid;

        my @statements = $self->driver->create_thread($thread);

        push @statements, $self->_process($root, $thread);

        # slurp up all the statements
        map { $self->model->add_statement($_) } @statements;
    }

    # return the number of messages processed
    scalar @messages;
}

=head2 attachments

Returns the list of attachment digest URIs (see L<URI::di>).

=cut

sub attachments {
    my $self = shift;
    return map { URI->new($_) } keys %{$self->_attach};
}

=head2 attachment $URI

Retrieves a SCALAR reference to the raw binary content of an
attachment. In list context, this method returns the attachment, its
MIME type, and a list of associated filenames (the same file may have
been attached to different emails using different names). In scalar
context, just the attachment reference itself:

    my $id = URI->new
        ('di:sha-256;AekWTPh53Qxi8rO53fjFQQbUl_CUN5t0q-2lyJHIqZ4');

    my $dataref = $emrdf->attachment($id) # also takes a string

    # Or, in list context:

    my ($dataref, $type, @names) = $emrdf->attachment($id);

    # Then you can do something like this:

    my ($ext) = File::MimeInfo::extensions($type);  # but smarter
                                                    # than this

    my $filename = sprintf('%s.%s', $id->b64digest, $ext);

    # or this, but I would probably want to write a map of these or
    # something:

    my $filename = $names[0];

    open my $fh, ">$filename";
    $fh->syswrite($$dataref);

=cut

sub attachment {
    my ($self, $id) = @_;
    return unless my $a = $self->_attach->{$id};
    wantarray ? @$a : $a->[0];
}

=head2 mint_uuid

=cut

sub mint_uuid {
    my $self = shift;
    my $uuid = OSSP::uuid->new;
    $uuid->make('v4');
    URI->new("urn:uuid:" . lc $uuid->export('str'));
}

=head2 serialize

=cut

sub serialize {
    my ($self, $kind, $fh) = @_;
    my $serializer = RDF::Trine::Serializer->new
        ($kind, namespaces => $self->driver->namespaces);

    my $string = '';
    unless (defined $fh) {
        # do what should be in RDF::Trine::Serializer
        open $fh, '>:utf8', \$string;
    }

    $serializer->serialize_model_to_file($fh, $self->model);
    return $string || 1;
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

    perldoc Email::RDF

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
L<http://www.apache.org/licenses/LICENSE-2.0>.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied.  See the License for the specific language governing
permissions and limitations under the License.

=cut

__PACKAGE__->meta->make_immutable;

1; # End of Email::RDF
