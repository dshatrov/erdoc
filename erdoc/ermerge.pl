#!/usr/bin/perl

#   erDoc - Documentation extractor for C/C++
#   Copyright (C) 2011 Dmitry Shatrov
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program; if not, write to the Free Software Foundation, Inc.,
#   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


use strict;
use XML::DOM;

binmode (STDIN,  ":utf8");
binmode (STDOUT, ":utf8");

my $out_doc = XML::DOM::Document->new;

# TODO: The comparisons seem to be based on getAttribute ('name'),
#       which is not set most of the time (or always).
#       Provided this, this script seems to be useless.
# Note: ermerge.pl is used for merging type catalogues,
#       so this requires further investigation.

sub merge_nontype_subnodes {
    my $node = shift @_;
    my $out_node = shift @_;

    for my $subnode ($node->getChildNodes ()) {
	if (not ($subnode->getNodeTypeName () eq 'ELEMENT_NODE' and
		 ($subnode->getTagName () eq 'global' or
		  $subnode->getTagName () eq 'namespace' or
		  $subnode->getTagName () eq 'class' or
		  $subnode->getTagName () eq 'struct' or
		  $subnode->getTagName () eq 'union' or
		  $subnode->getTagName () eq 'enum')))
# Unnecessary?
#		  $subnode->getTagName () eq 'category' or
#		  $subnode->getTagName () eq 'root')))
	{
	    my $out_subnode = $subnode->cloneNode (1);
	    $out_subnode->setOwnerDocument ($out_doc);
	    $out_node->appendChild ($out_subnode);
	}
    }
}

sub merge_type {
    my $node = shift @_;
    my $out_cur = shift @_;

    my $name = $node->getAttribute ('name');

    my $out_dest;
    for my $out_node ($out_cur->getChildNodes ()) {
	next if not $out_node->getNodeTypeName () eq 'ELEMENT_NODE';

	if ($out_node->getTagName () eq $node->getTagName ()) {
	    my $out_name = $out_node->getAttribute ('name');

	    if ($out_name eq $name) {
		# $out_dest acts like a flag
		$out_dest = $out_node;
		# UNNECESSARY merge_nontype_subnodes ($node, $out_dest);
		last;
	    }
	}
    }

    if (not $out_dest) {
	$out_dest = $node->cloneNode (0);
	$out_dest->setOwnerDocument ($out_doc);
	merge_nontype_subnodes ($node, $out_dest);
	$out_cur->appendChild ($out_dest);
    }

    merge_types ($node, $out_dest);
}

sub merge_types {
    my $node = shift @_;
    my $out_cur = shift @_;

    for my $subnode ($node->getChildNodes ()) {
	next if not $subnode->getNodeTypeName () eq 'ELEMENT_NODE';

	my $tag = $subnode->getTagName ();
	if ($tag eq 'global' or
	    $tag eq 'namespace' or
	    $tag eq 'class' or
	    $tag eq 'struct' or
	    $tag eq 'union' or
	    $tag eq 'enum')
# Unnecessary?
#	    $tag eq 'category' or
#	    $tag eq 'root')
	{
	    merge_type ($subnode, $out_cur);
	}
    }
}

my @sources;
my $i = 0;
for my $source_filename (@ARGV) {
    my $parser = XML::DOM::Parser->new;
    my $doc = $parser->parsefile ($ARGV [$i]);
    $sources [$i] = $doc;
    $i ++;
}

my $out_root = $out_doc->createElement ('ertypes');
for my $source (@sources) {
    my $root = $source->getDocumentElement ();
    merge_types ($root, $out_root);
}

print $out_root->toString ();

