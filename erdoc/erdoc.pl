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

# In "cat" mode we create a new 'ertyes' XML file and print it
# to the standard output.
#
# In "sp_cat" mode we create links between related types (inheritance)
# in an alreay created types.xml.

my $cat_mode;
if ($ARGV [0] eq 'cat') {
    $cat_mode = 1;
}

my $sp_cat_mode;
if ($ARGV [0] eq 'sp') {
    $cat_mode = 1;
    $sp_cat_mode = 1;
}

# Used only in "cat" mode.
my $path_prefix;
if ($cat_mode) {
    $path_prefix = $ARGV [1];
    $path_prefix = "../$path_prefix/" if $path_prefix;
}

my $doc;
my $root;

my $typecat;
my $typecat_root;

my $xml_pi;
my $xml_pi_xsl;

{
  # We always create out.xml root, because it is used for
  # tracking categories, which is necessary for generating
  # proper type ref's ('ref' attributes).

    $doc = XML::DOM::Document->new;
    $root = $doc->createElement ('erdoc');

    $xml_pi = $doc->createXMLDecl ('1.0', 'UTF-8');
    $xml_pi_xsl = $doc->createProcessingInstruction (
			    'xml-stylesheet',
			    'type="text/xsl" href="erdoc.xsl"');
}

if ($cat_mode and
    not $sp_cat_mode)
{
    $typecat = XML::DOM::Document->new;
    $typecat_root = $typecat->createElement ('ertypes');
} else {
    my $parser = XML::DOM::Parser->new;
    $typecat = $parser->parsefile ('types.xml');
    $typecat_root = $typecat->getDocumentElement ();
}

push_ns ($typecat_root);

# .open/.close nesting counter
# Used for scoping "using namespace" directives (.using mark).
my $cur_level = 0;

my @usings;

my %type_map;

my @classes;
my %derivatives;

sub parse_erl_string {
    my $string = shift @_;

    my $parser = XML::DOM::Parser->new;
#    print STDERR "parse_erl_string: $string\n";
    my $str_doc = $parser->parse ("<erl>$string</erl>");
    my $str_root = $str_doc->getDocumentElement ();
    $str_root->setOwnerDocument ($doc);

    return $str_root;
}

# Type search.
# [0] - type tree node
# [1] - qualified type name in the form of an array
sub lookup_type_node {
    my $top = shift @_;
    my $name_arr = shift @_;

    $top = $typecat_root if not $$name_arr [0];

    my $node = 0;

    my $i;
    for ($i = 0; $i < @$name_arr; $i++) {
	next if not $$name_arr [$i];

	my $next_node = 0;
	for $node ($top->getChildNodes ()) {
	    next if not $node->getNodeTypeName () eq 'ELEMENT_NODE';
	    if ($node->getAttribute ('name') eq $$name_arr [$i]) {
		$next_node = $node;
		last;
	    }
	}

	last if not $next_node;
	$top = $next_node;
	$node = $next_node;
    }

    return 0 if $i < @$name_arr;

    return $node;
}

sub get_type_node {
    my $bottom = shift @_;
    my $name_arr = shift @_;

    my $ns = $bottom;
    while ($ns) {
	last if not $ns->getNodeTypeName () eq 'ELEMENT_NODE';

	my $node = lookup_type_node ($ns, $name_arr);
	return $node if $node;

	last if $ns->getTagName () eq 'ertypes';

	for my $subnode ($ns->getChildNodes ()) {
	    next if not $subnode->getNodeTypeName () eq 'ELEMENT_NODE' or
		    not $subnode->getTagName () eq 'parent';

	    my $parent_name = $subnode->getAttribute ('type');
	    my @parent_arr = split_type_name ($parent_name);
	    my $parent_node = get_type_node ($ns->getParentNode (), \@parent_arr);
	    next if not $parent_node;

	    $node = get_type_node ($parent_node, $name_arr);
	    return $node if $node;
	}

	$ns = $ns->getParentNode ();
    }

    for my $using (@usings) {
	my $node = lookup_type_node ($$using [1], $name_arr);
	return $node if $node;
    }

    return 0;
}

sub get_type_ref {
    my $bottom = shift @_;
    my $name_arr = shift @_;

    my $node = get_type_node ($bottom, $name_arr);

    return 0 if not $node;
    return $node->getAttribute ('ref');
}

sub get_ns_depth {
    my $ns = shift @_;

    my $depth = 0;
    while (not $ns->getTagName () eq 'ertypes') {
	$ns = $ns->getParentNode ();
	$depth ++;
    }

    return $depth - 1;
}

sub process_inheritance_rec {
    my $ns = shift @_;
    my $depth = shift @_;
    my $parents = shift @_;
    my $virtuals = shift @_;

    for my $subnode ($ns->getChildNodes ()) {
	next if not $subnode->getNodeType () == ELEMENT_NODE or
		not $subnode->getTagName () eq 'parent';

	my $name = $subnode->getAttribute ('type');
	my @name_arr = split_type_name ($name);
	my $parent_ns = get_type_node ($ns->getParentNode (), \@name_arr);

	my $parent_type_name = get_full_type_name ($parent_ns);

	if ($parent_ns and
	    $subnode->getAttribute ('virtual'))
	{
	    my $in_virtuals;
	    for my $virtual (@$virtuals) {
		if ($virtual eq $parent_type_name) {
		    $in_virtuals = 1;
		    last;
		}
	    }

	    if ($in_virtuals) {
		next;
	    } else {
		$$virtuals [@$virtuals] = $parent_type_name;
	    }
	}

	if ($parent_ns) {
	    process_inheritance_rec ($parent_ns, $depth + 1, $parents, $virtuals);
	}

	my $parent = $doc->createElement ('parent');
	$parent->setAttribute ('depth', $depth);

	if ($parent_ns) {
	    my $type = $doc->createElement ('type');
	    # TODO Strip name elements belonging to the current namespace
	    $type->setAttribute ('name', $parent_type_name);
	    $type->setAttribute ('ref', $parent_ns->getAttribute ('ref'));
	    $parent->appendChild ($type);
	} else {
	    $parent->addText ($name);
	}

	$$parents [@$parents] = $parent;
    }
}

sub process_inheritance {
    my $ns = shift @_;
    my $class = shift @_;

    my @parents;
    my @virtuals;
    process_inheritance_rec ($ns, 0, \@parents, \@virtuals);

    if (scalar (@parents)) {
	my $parents = $doc->createElement ('parents');
	for my $parent (@parents) {
	    $parents->appendChild ($parent);
	}

	$class->appendChild ($parents);
    }
}

sub process_derivatives {
    my $ns = shift @_;
    my $class = shift @_;

    for my $child ($ns->getChildNodes ()) {
	next if not $child->getNodeType () eq ELEMENT_NODE or
		not $child->getTagName () eq 'parent';

	if ($child->getAttribute ('qualified') eq '1') {
	    my $parent_name = $child->getAttribute ('type');

	    my $dref = $derivatives {$parent_name};
	    if (not $dref) {
		my @darr;
		$derivatives {$parent_name} = \@darr;
		$dref = \@darr;
	    }

	    $$dref [@$dref] = get_full_type_name ($ns);
	}
    }
}

sub postprocess_derivatives {
    for my $class (@classes) {
	my $dref = $derivatives {$$class [0]};
	if ($dref) {
	    my $derivatives = $doc->createElement ('derivatives');
	    $$class [1]->appendChild ($derivatives);

	    for my $der_name (@$dref) {
		my $derivative = $doc->createElement ('derivative');
		$derivatives->appendChild ($derivative);

		my @der_name_arr = split_type_name ($der_name);
		my $ns = lookup_type_node ($typecat_root, \@der_name_arr);

		if ($ns) {
		    my $type = $doc->createElement ('type');
		    # TODO Strip name elements belonging to the current namespace
		    $type->setAttribute ('name', $der_name);
		    $type->setAttribute ('ref', $ns->getAttribute ('ref'));
		    $derivative->appendChild ($type);
		} else {
		    $derivative->addText ($der_name);
		}
	    }
	}
    }
}

sub type_postcreate {
    my $class = shift @_;
    my $ns = shift @_;

    my @class_record = (get_full_type_name ($ns), $class);
    $classes [@classes] = \@class_record;
    process_derivatives ($ns, $class);

    process_inheritance ($ns, $class);

    type_postcreate_common ($class, $ns);
}

sub type_postcreate_common {
    my $class = shift @_;
    my $ns = shift @_;

    my $type = $class->getTagName ();
    my $node = $class->getParentNode ();
    while ($node) {
	my $has_hmark = 0;
	my $child = $node->getFirstChild ();
	while ($child) {
	    if ($child->getTagName () eq "has_$type") {
		$has_hmark = 1;
		last;
	    }

	    $child = $child->getNextSibling ();
	}

	last if $has_hmark;

	my $hmark = $doc->createElement ("has_$type");
	$node->appendChild ($hmark);

	last if $node->getTagName () eq 'erdoc';

	$node = $node->getParentNode ();
    }

    my $filename = $doc->createElement ('filename');
    $class->appendChild ($filename);
    {
	my $filename_text;

# (commented out) This is (probably) wrong!
#	if (ns_is_namespace ($ns)) {
#	  # For global types, we set category's filename,
#	  # or index.html (if there's no category).
#
#	    my $category = get_category ();
#	    if ($category) {
#		$filename_text = get_category_name ($category);
#	    } else {
#		$filename_text = 'index';
#	    }
#	} else {
	    if ($ns->getAttribute ('primary')) {
#		$filename->addText ($ns->getAttribute ('primary').'.html');
		$filename_text = $ns->getAttribute ('primary');

		my $primary = $doc->createElement ('primary');
		$primary->addText ($ns->getAttribute ('primary'));
		$class->appendChild ($primary);
	    } else {
#		$filename->addText (get_typename_prefix ($ns).'.html');
		$filename_text = get_typename_prefix ($ns);
	    }
#	}

	$filename->addText ($filename_text.'.html');
    }

    my $depth = $doc->createElement ('depth');
    $class->appendChild ($depth);
    $depth->addText (get_ns_depth ($ns));
}

sub ensure_ns_mapping {
    my $name = shift @_;

    my @name_arr = split_type_name ($name);
    return lookup_type_node (get_ns (), \@name_arr);
}

sub enforce_type_mapping {
    my $type_node = shift @_;

    return $type_map {$type_node} if $type_map {$type_node};

    my @type_branch;
    my $ns = $type_node;
    while ($ns) {
	last if $ns->getTagName () eq 'ertypes';

	my @branch_node = ($ns->getTagName (), $ns->getAttribute ('name'), $ns);
	$type_branch [@type_branch] = \@branch_node;

	$ns = $ns->getParentNode ();
    }

    my $node = $root;
    for (my $i = @type_branch; $i > 0; $i --) {
	my $branch_node = $type_branch [$i - 1];

	my $next_node;
	for my $subnode ($node->getChildNodes ()) {
	    if ($subnode->getNodeType () == ELEMENT_NODE and
		$subnode->getTagName () eq $$branch_node [0])
	    {
		my $name;
		for my $child ($subnode->getChildNodes ()) {
		    if ($child->getNodeType () == ELEMENT_NODE and
			$child->getTagName () eq 'name')
		    {
			for my $subchild ($child->getChildNodes ()) {
			    if ($subchild->getNodeType () == TEXT_NODE) {
				$name = $subchild->getNodeValue ();
			    }

			    # FIXME last if == TEXT_NODE?
			    last;
			}

			last;
		    }
		}

		if ($name eq $$branch_node [1]) {
		    $next_node = $subnode;
		}
	    }

	    next;
	}

	if (not $next_node) {
	    $next_node = $doc->createElement ($$branch_node [0]);
	    $node->appendChild ($next_node);

	    my $name = $doc->createElement ('name');
	    $next_node->appendChild ($name);
	    $name->addText ($$branch_node [1]);

	    my $full_name = $doc->createElement ('full_name');
	    $next_node->appendChild ($full_name);
	    $full_name->addText (get_full_type_name ($type_node));

	    $type_map {$$branch_node [2]} = $next_node;

	    type_postcreate ($next_node, $$branch_node [2]);
	}

	$node = $next_node;
    }

    return $node;
}

sub enforce_category_mapping {
    my $category  = shift @_;
    my $type_node = shift @_;

    my @type_branch;
    my $ns = $type_node;
    while ($ns) {
	last if $ns->getTagName () eq 'ertypes';

	my @branch_node = ($ns->getTagName (), $ns->getAttribute ('name'), $ns);
	$type_branch [@type_branch] = \@branch_node;

	$ns = $ns->getParentNode ();
    }

    my $node = $category;
    for (my $i = @type_branch; $i > 0; $i --) {
	my $branch_node = $type_branch [$i - 1];

	my $next_node;
	for my $subnode ($node->getChildNodes ()) {
	    if ($subnode->getNodeType () == ELEMENT_NODE and
		$subnode->getTagName () eq $$branch_node [0])
	    {
		my $name;
		for my $child ($subnode->getChildNodes ()) {
		    if ($child->getNodeType () == ELEMENT_NODE and
			$child->getTagName () eq 'name')
		    {
			for my $subchild ($child->getChildNodes ()) {
			    if ($subchild->getNodeType () == TEXT_NODE) {
				$name = $subchild->getNodeValue ();
			    }

			    last;
			}

			last;
		    }
		}

		if ($name eq $$branch_node [1]) {
		    $next_node = $subnode;
		}
	    }

	    next;
	}

	if (not $next_node) {
	    $next_node = $doc->createElement ($$branch_node [0]);
	    $node->appendChild ($next_node);

	    my $name = $doc->createElement ('name');
	    $next_node->appendChild ($name);
	    $name->addText ($$branch_node [1]);

	    my $full_name = $doc->createElement ('full_name');
	    $next_node->appendChild ($full_name);
	    $full_name->addText (get_full_type_name ($type_node));

	    type_postcreate_common ($next_node, $$branch_node [2]);
	}

	$node = $next_node;
    }

    return $node;
}

sub get_full_type_name {
    my $bottom = shift @_;

    my $prefix;
    my $ns = $bottom;
    while ($ns) {
	last if $ns->getTagName () eq 'ertypes';

	$prefix = '::'.$prefix if $prefix;
	$prefix = $ns->getAttribute ('name').$prefix;

	$ns = $ns->getParentNode ();
    }

    return $prefix;
}

sub get_typename_prefix {
    my $bottom = shift @_;

    my $prefix;
    my $ns = $bottom;
    while ($ns) {
	last if $ns->getTagName () eq 'ertypes';

	$prefix = '.'.$prefix if $prefix;
	$prefix = $ns->getAttribute ('name').$prefix;

	$ns = $ns->getParentNode ();
    }

    return $prefix;
}

sub split_type_name {
    my $type_name = shift @_;

    my @type_arr;
    my $i = 0;
    while ($type_name =~ /(?:^\s*|(~?\w+))(?:::|$)/g) {
	$type_arr [$i] = $1;
	$i ++;
    }

    return @type_arr;
}

sub get_namespace_name {
    my $decl = shift @_;

    if ($decl =~ /(?:^|\s)namespace\s+(\w+)/) {
	return $1;
    } else {
	return "NAMESPACE";
    }
}

sub get_class_name {
    my $decl = shift @_;

    if ($decl =~ /^\s*(?:template\s*<[^<>]+>)?.*?(?:class|struct|union|enum|namespace)\s+(\w+)/s) {
	return $1;
    } else {
	print STDERR "DECL: $decl\n";
	return "CLASS";
    }
}

sub retab_decl {
    my $decl = shift @_;

    my $tab_spaces = ' ' x 8;
    $decl =~ s/\t/$tab_spaces/sg;

    # Minimum preceding whitespace length
    my $min_ts;
    my $min_ts_set = 0;
    while ($decl =~ /^(\s*)/mgc) {
	my $ts = length ($1);

	if ($min_ts_set) {
	    $min_ts = $ts if $min_ts > $ts;
	} else {
	    $min_ts = $ts;
	    $min_ts_set = 1;
	}
    }

    my $trailer = ' ' x $min_ts;
    $decl =~ s/^$trailer//mg;

    return $decl;
}

my @popmark_stack;

sub push_popmark__category {
    $popmark_stack [@popmark_stack] = 1;
}

sub push_popmark__toplevel_ns {
    $popmark_stack [@popmark_stack] = 2;
}

sub pop_popmark {
    pop_category () if $popmark_stack [@popmark_stack - 1] & 1;

    pop_ns ()       if $popmark_stack [@popmark_stack - 1] & 2;
    pop_toplevel () if $popmark_stack [@popmark_stack - 1] & 2;

    $#popmark_stack --;
}

my @category_stack;

sub get_category {
    return $category_stack [$#category_stack];
}

sub push_category {
    my $category = shift @_;

    $category_stack [@category_stack] = $category;
}

sub pop_category {
    $#category_stack --;
}

my @ns_stack;

sub get_ns {
    return $ns_stack [$#ns_stack];
}

sub push_ns {
    my $ns = shift @_;

    $ns_stack [@ns_stack] = $ns;
}

sub pop_ns {
    $#ns_stack --;
}

sub ns_is_namespace {
    my $ns = shift @_;

    if ($ns->getTagName () eq 'ertypes' or
	$ns->getTagName () eq 'namespace')
    {
	return 1;
    }

    return 0;
}

my @toplevel_stack;

# Get current toplevel
sub get_ct {
    return $toplevel_stack [$#toplevel_stack];
}

sub push_toplevel {
    my $toplevel = shift @_;

    $toplevel_stack [@toplevel_stack] = $toplevel;
}

sub pop_toplevel {
    $#toplevel_stack --;
}

sub ct_set_parents {
    my $parents = shift @_;

    my $toplevel = get_ct ();
    $$toplevel {parents} = $parents;
}

sub ct_get_parents {
    my $toplevel = get_ct ();
    return $$toplevel {parents};
}

sub ct_add_child {
    my $child = shift @_;

    my $toplevel = get_ct ();
    $$toplevel {children} [$$toplevel {child_i} ++] = $child if $child;
}

sub ct_parentize_children {
    my $parent = shift @_;

    my $toplevel = get_ct ();
    for (my $i = 0; $i < $$toplevel {child_i}; $i++) {
# TODO Filtering
#	if ($children [$i]->getTagName () eq 'desc') {
	    $parent->appendChild ($$toplevel {children} [$i]);
#	}
    }
}

sub ct_parentize_children_copy {
    my $parent = shift @_;

    my $toplevel = get_ct ();
    for (my $i = 0; $i < $$toplevel {child_i}; $i++) {
# TODO Filtering
#	if ($children [$i]->getTagName () eq 'desc') {
	    $parent->appendChild ($$toplevel {children} [$i]->cloneNode (1));
#	}
    }
}

sub ct_set_name {
    my $name_str = shift @_;

    my $toplevel = get_ct ();
    if (not $$toplevel {name}) {
	print STDERR "--- SETTING NAME: $name_str\n";

	$$toplevel {name} = $name_str;
    }
}

sub ct_get_name {
    my $toplevel = get_ct ();
    return $$toplevel {name};
}

sub ct_set_is_template {
    my $is_template = shift @_;

    my $toplevel = get_ct ();
    $$toplevel {is_template} = $is_template;
}

sub ct_get_is_template {
    my $toplevel = get_ct ();
    return $$toplevel {is_template};
}

sub ct_set_decl_handler {
    my $handler = shift @_;

    my $toplevel = get_ct ();
    $$toplevel {decl_handler} = $handler;
}

sub ct_get_decl_handler {
    my $toplevel = get_ct ();
    return $$toplevel {decl_handler};
}

sub ct_set_primary {
    my $primary = shift @_;

    my $toplevel = get_ct ();
    $$toplevel {primary} = $primary;
}

sub ct_get_primary {
    my $toplevel = get_ct ();
    return $$toplevel {primary};
}

sub parse_name {
    print STDERR "--- parse_name\n";

    my $args = shift @_;
    my $body = shift @_;

    if ($args =~ /([\w:]+)/) {
	print STDERR "--- parse_name: $1\n";

	ct_set_name ($1);
    }
}

sub add_global_to_category {
    my $ns = shift @_;
    my $type_node = shift @_;

    if (ns_is_namespace ($ns)) {
	my $category = get_category ();
	if ($category) {
	    $category->appendChild ($type_node->cloneNode (1));
	}
    }
}

# Extracts abstract and/or description from an erdoc description block.
#
# Flags:
#     $task & 1 - create an abstract
#     $task & 2 - create a description
sub parse_desc {
    my $desc_str = (shift @_).(shift @_);
    my $task = shift @_;

    $desc_str =~ s/^\s*//s;
    $desc_str =~ s/\s*$//s;
    if ($desc_str) {
	if ($task & 1 and
	    $desc_str =~ /(.*?)(?:\n\s*\n|$)/sgc)
	{
	    my $abstract = $doc->createElement ('abstract');
	    $abstract->appendChild (parse_erl_string ($1));
	    ct_add_child ($abstract);
	}

	if ($task & 2) {
	    my $desc = $doc->createElement ('desc');
	    my $got_desc = 0;
	    while ($desc_str =~ /(.+?)(?:\n\s*\n|$)/sgc) {
	      # We split the description into paragraphs (<p> tags).

		my $str = $1;
		# '|' at the beginning of the line protects from
		# stripping whitespace at the beginning of the line,
		# and from splitting into paragraphs.
		$str =~ s/^\| ?//mg;
		$desc->appendChild (parse_erl_string ("<p>$str</p>"));
		$got_desc = 1;
	    }

	    ct_add_child ($desc) if $got_desc;
	}
    }
}

sub add_parent {
    my $virtual = shift @_;
    my $protection = shift @_;
    my $name = shift @_;

    my $parent = $typecat->createElement ('parent');

    my @name_arr = split_type_name ($name);
    my $ns = get_type_node (get_ns (), \@name_arr);
    if ($ns) {
	$name = get_full_type_name ($ns);
	$parent->setAttribute ('qualified', '1');
    }

    $parent->setAttribute ('protection', $protection);
    # Better to say "type" because it can be qualified
    $parent->setAttribute ('type', $name);
    if ($virtual) {
	$parent->setAttribute ('virtual', '1');
    }

    my $parents = ct_get_parents ();
    $$parents [@$parents] = $parent;
}

# Used only in "sp_cat" mode.
sub process_parent_decl {
    my $decl = shift @_;

    $decl =~ /\s*(virtual\s+)?(public|protected|private)?(\s+virtual)?\s+([\w:]+)/s;
    my $protection = $2;
    my $name = $4;

    my $virtual;
    if ($1 or $3) {
	$virtual = 1;
    }

    $protection = 'private' if not $protection;

    return if not $name;

    add_parent ($virtual, $protection, $name);
}

sub parse_class_decl {
    my $decl_str = shift @_;

    if (my $name_str = get_class_name ($decl_str)) {
	ct_set_name ($name_str);
    }

    if ($decl_str =~ /^\s*template\s*<[^<>]*>/s) {
	ct_set_is_template (1);
    }

    if ($sp_cat_mode) {
	my $parents = ct_get_parents ();

	if ($decl_str =~ /[^:]:([^:].*)/s) {
	    my $parents_decl = $1;

	    my $re;
	    $re = qr/(?:[^<>]+|<(??{$re})>)*/;
	    while ($parents_decl =~ /\G((?:[^<>,]+|<$re>)+)(?:,|$)/sgc) {
		my $parent_decl = $1;

		process_parent_decl ($parent_decl);
	    }
	}
    }

    if (not $cat_mode) {
	my $decl = $doc->createElement ('decl');
	$decl_str =~ s/^\s*\n//s;
	$decl_str =~ s/\s*$//s;
	$decl_str = retab_decl ($decl_str);
	$decl->addText ($decl_str);
	ct_add_child ($decl);
    }
}

sub parse_method_decl {
    my $decl_str = shift @_;

    $decl_str =~ s/^\n*//s;
    $decl_str =~ s/\n*$//s;
    $decl_str = retab_decl ($decl_str);
    if ($decl_str) {
	my $decl;
	if (not $cat_mode) {
	    $decl = $doc->createElement ('decl');
	}

	# Internal parentheses may be in default values' initializers,
	# so we have to make sure they are nested properly.
	my $re;
	$re = qr/(?:[^\(\)]*|\((??{$re})\))*/;
# Deprecated		      ((?:\(\s*\*?)?\s*?(?:operator\s+[^\s]+|[^\(\)\s]*)\s*?\)?\s*?)
#		      (\(?\s*?(?:operator\s+[^\s]+|[^\(\)\s]*)\s*?\)?\s*?)
	if ($decl_str =~
		    /^(\s*[^\(\)]*?\s*)
		      (\(?\s*?(?:operator\s+[^\s]+|[^\(\)\s]*)\s*?\)?\s*?)
		      (?:(\()($re\)\s*(?:[^\(\)]*?|[^\(\)\s].*?)))?
		      \s*$
		    /sx)
	{
	    my $rettype_str = $1;
	    my $symbol_str = $2;
	    my $arglist_prefix_str = $3;
	    my $arglist_str = $4;

	    print STDERR "--- rettype: $rettype_str\n";
	    print STDERR "--- SYMBOL: $symbol_str\n";

	    if ($symbol_str =~ /^\s*\(/) {
		print STDERR "--- FORCE SYMBOL: $symbol_str\n";
		ct_set_name ($symbol_str);
	    }

# for
	    # Note that this regexp matches any valid method name,
	    # not only operators.
	    if ($symbol_str =~ /(operator\s+[^\s]+|~?[\w:]+)/) {
		print STDERR "--- BOO!\n";
# TEST (uncomment)
		ct_set_name ($1);
	    }
# cut

	    if (not $cat_mode) {
		my $rettype = $doc->createElement ('rettype');
		parse_type_decl ($rettype_str, $rettype);
		ct_add_child ($rettype);

		my $symbol = $doc->createElement ('symbol');
		$symbol_str =~ /^\(?\s*(.*?)\s*\)?$/s;
#		$symbol_str =~ /^\s*(.*?)\s*$/s;
		$symbol->addText ($1);
		ct_add_child ($symbol);

		my $arglist_prefix = $doc->createElement ('arglist_prefix');
		$arglist_prefix->addText ($arglist_prefix_str);
		ct_add_child ($arglist_prefix);

		my $arglist = parse_arglist ($arglist_str);
		ct_add_child ($arglist);

		$rettype = $rettype->cloneNode (1);
		$decl->appendChild ($rettype);

		# FIXME $symbol is used twice, this is confusing.
		$symbol_str =~ /^(\(?\s*)(.*?)(\s*\)?)$/s;
#		$symbol_str =~ /^(\s*)(.*?)(\s*)$/s;
		$decl->addText ($1);
		my $symbol = $doc->createElement ('symbol');
		$symbol->addText ($2);
		$decl->appendChild ($symbol);
		$decl->addText ($3);

		$arglist = $arglist->cloneNode (1);
		$decl->addText ($arglist_prefix_str);
		$decl->appendChild ($arglist);
	    }
	} else {
	    if (not $cat_mode) {
		$decl->addText ($decl_str);
	    }
	}

	if (not $cat_mode) {
	    ct_add_child ($decl);
	}
    }
}

sub parse_type_token {
    my $token_str = shift @_;
    my $parent = shift @_;

    if ($token_str =~
	    /(^\s*|.*?[^\w])
	     (void     |
	      char     |
	      short    |
	      int      |
	      long     |
	      unsigned |
	      long     |
	      float    |
	      double   |
	      class    |
	      struct   |
	      union    |
	      enum)
	     (\s*$|[^\w].*?)
	    /sgx)
    {
	$parent->addText ($1);

	my $basic = $doc->createElement ('basic');
	$basic->addText ($2);
	$parent->appendChild ($basic);

	$parent->addText ($3);
	return;
    }

    if ($token_str =~
	    /(^\s*|.*?[^\w])
	     (const    |
	      static   |
	      virtual  |
	      volatile |
	      register |
	      typedef  |
	      throw)
	     (\s*$|[^\w].*?)
	    /sgx)
    {
	$parent->addText ($1);

	my $basic = $doc->createElement ('keyword');
	$basic->addText ($2);
	$parent->appendChild ($basic);

	$parent->addText ($3);
	return;
    }

    if ($token_str =~
	    /(^\s*|.*?[^\w:])
	     ([\w:]+)
	     (\s*$|[^\w:].*?)
	    /sgx)
    {
	$parent->addText ($1);

	my $type_name = $2;
	my @type_name = split_type_name ($type_name);
	my $type_ref = get_type_ref (get_ns (), \@type_name);
	if ($type_ref) {
	    my $type = $doc->createElement ('type');
	    $type->setAttribute ('name', $type_name);
	    $type->setAttribute ('ref', $type_ref);
	    $parent->appendChild ($type);
	} else {
	    $parent->addText ($type_name);
	}

	$parent->addText ($3);
	return;
    }

    $parent->addText ($token_str);
}

sub parse_type_decl {
    my $decl_str = shift @_;
    my $parent = shift @_;

    while ($decl_str =~
	    /([^\w:]*)
	     ([\w:]*)
	     ([^\w:]*)
	    /sgcx)
    {
	$parent->addText ($1);
	parse_type_token ($2, $parent);
	$parent->addText ($3);
    }
 }

sub parse_arglist {
    my $decl_str = shift @_;

    my $arglist = $doc->createElement ('arglist');

    my $re;
    $re = qr/(?:[^<>]+|<(??{$re})>)*/;
    while ($decl_str =~ /\G((?:[^<>,]+|<$re>)+)(,|$)/sgc) {
	my $type_decl = $doc->createElement ('type_decl');
	$arglist->appendChild ($type_decl);
	parse_type_decl ($1, $type_decl);

	$arglist->addText ($2);
    }

    if ($decl_str =~ /(.*)/sgc) {
	$arglist->addText ($1);
    }

    return $arglist;
}

sub parse_type_member_decl {
    my $decl_str = shift @_;

    parse_method_decl ($decl_str);
}

sub parse_data_decl {
    print STDERR "--- parse_data_decl\n";

    my $decl_str = shift @_;

    $decl_str =~ s/^\n*//s;
    $decl_str =~ s/\n*$//s;
    $decl_str = retab_decl ($decl_str);
    $decl_str =~ s/[,;]\s*//mg;

    if ($decl_str and
	$decl_str =~ /(.*?)([*&]*[_[:alpha:]]\w*)(?:\s*=.*)?\s*$/s)
    {
	print STDERR "--- OK: $2\n";

	ct_set_name ($2);

	my $type = $doc->createElement ('data_type');
	parse_type_decl ($1, $type);
	ct_add_child ($type);
    } else {
	ct_set_name ("DATA");
    }
}

sub parse_decl {
    my $decl_str = (shift @_).(shift @_);

    if ($decl_str) {
	&{ct_get_decl_handler ()} ($decl_str);
    }
}

sub parse_methods {
    my $methods_str = (shift @_).(shift @_);

    if ($methods_str) {
	my $methods = $doc->createElement ('methods');
	$methods->appendChild (parse_erl_string ($methods_str));
	ct_add_child ($methods);
    }
}

sub parse_data_fields {
    my $df_str = (shift @_).(shift @_);

    if ($df_str) {
	my $df = $doc->createElement ('data_fields');
	$df->appendChild (parse_erl_string ($df_str));
	ct_add_child ($df);
    }
}

sub parse_types {
    my $df_str = (shift @_).(shift @_);

    if ($df_str) {
	my $df = $doc->createElement ('types');
	$df->appendChild (parse_erl_string ($df_str));
	ct_add_child ($df);
    }
}

sub parse_title {
    my $title_str = (shift @_).(shift @_);

    if ($title_str) {
	my $title = $doc->createElement ('title');
	$title->addText ($title_str);
	ct_add_child ($title);
    }
}

sub parse_arg {
    my $args = shift @_;
    my $body = shift @_;

    if ($args =~ /([\w\/]+)(\s+(.*))?/) {
	my $arg_name = $1;
	my $arg_desc = $2;

	$arg_desc .= $body;

	my $arg = $doc->createElement ('arg');
	ct_add_child ($arg);

	my $name = $doc->createElement ('name');
	$arg->appendChild ($name);
	$name->addText ($arg_name);

	my $desc = $doc->createElement ('desc');
	$desc->appendChild (parse_erl_string ($arg_desc));
	$arg->appendChild ($desc);
    }
}

sub parse_primary {
    my $args = shift @_;
    my $body = shift @_;

    if ($args =~ /([\w:]+)/) {
	ct_set_primary ($1);
    }
}

# Used to parse explicit ".parent" blocks.
#
# TODO This looks very similar to process_parent_decl(). Unite?
sub parse_parent {
    my $args = shift @_;
    my $body = shift @_;

    if ($args =~ /(\w+)(?:\s+([\w:]+))?(?:\s+(.*)|$)/) {
	my $protection = $1;
	my $parent_name = $2;

	# FIXME Honor virtuals.
	add_parent (undef, $protection, $parent_name);
    }
}

sub parse_body {
    my $block = shift @_;

    if ($$block =~ /(.*?)(?=\n\s*(?:\.\w|@)|$)/sgc and
	not $cat_mode)
    {
	parse_desc ('', $1, 3);
    }

    while ($$block =~ /\n\s*(\.\w+|@)(?:(?:[ \t]+|(?<=@))([^\n]*))?[ \t]*(?=\n|$)/sgc) {
	my $type = $1;
	my $args = $2;

	my $body;
	if ($$block =~ /(.*?)(?=\n\s*(?:\.\w|@)|$)/sgc) {
	    $body = $1;
	}

	if ($sp_cat_mode and
	    $type eq '.parent')
	{
	    parse_parent ($args, $body);
	} elsif ($type eq '.name') {
	    parse_name ($args, $body);
	} elsif (not $cat_mode and
		 $type eq '.desc')
	{
	    parse_desc ($args, $body, 2);
	} elsif (not $cat_mode and
		 $type eq '.abstract')
	{
	    parse_desc ($args, $body, 1);
	} elsif ($type eq '.decl') {
	    parse_decl ($args, $body);
	} elsif (not $cat_mode and
		 $type eq '.methods')
	{
	    parse_methods ($args, $body);
	} elsif (not $cat_mode and
		 $type eq '.data_fields')
	{
	    parse_data_fields ($args, $body);
	} elsif (not $cat_mode and
		 $type eq '.types')
	{
	    parse_types ($args, $body);
	} elsif (not $cat_mode and
		 $type eq '.title')
	{
	    parse_title ($args, $body);
	} elsif (not $cat_mode and
		 ($type eq '.arg' or
		  $type eq '@'))
	{
	    parse_arg ($args, $body);
	} elsif ($type eq '.primary') {
	    parse_primary ($args, $body);
	} elsif ($type eq '.end') {
	    # A hack to allow '.end's within a class block
	    return 1;
	}
    }

    return 0;
}

sub parse_class {
    my $args = shift @_;
    my $body = shift @_;
    my $type = shift @_;

    my $parent_toplevel = get_ct ();
    push_popmark__toplevel_ns ();

    my %toplevel;
    push_toplevel (\%toplevel);
    ct_set_decl_handler (\&parse_class_decl);

    if ($sp_cat_mode) {
	my @parents;
	ct_set_parents (\@parents);
    }

    if ($args =~ /([\w:]+)/) {
	ct_set_name ($1);
    }

    my $got_end = parse_body (\$body);

    my $name_str = ct_get_name ();
    my @class_name_arr = split_type_name ($name_str);

    if ($cat_mode) {
	my $ns_name = $class_name_arr [$#class_name_arr];
	$#class_name_arr --;

	my $parent;
	if (scalar (@class_name_arr)) {
	    $parent = lookup_type_node (get_ns (), \@class_name_arr);
	} else {
	    $parent = get_ns ();
	}

	$parent = $typecat_root if not $parent;

	my $new_ns;
	for my $node ($parent->getChildNodes ()) {
	    if ($node->getAttribute ('name') eq $ns_name) {
		$new_ns = $node;
		last;
	    }
	}

	if (not $new_ns) {
	    $new_ns = $typecat->createElement ($type);
	    $new_ns->setAttribute ('name', $ns_name);
	    $parent->appendChild ($new_ns);

	    $new_ns->setAttribute ('ref', $path_prefix.get_typename_prefix ($new_ns).'.html');
	}

	if (my $primary_name = ct_get_primary ()) {
	    $new_ns->setAttribute ('primary', $primary_name);
	    $new_ns->setAttribute ('ref', $path_prefix.$primary_name.'.html');

	    if (not $sp_cat_mode) {
		my $primary_ns = $typecat->createElement ($type);
		$primary_ns->setAttribute ('name', $primary_name);
		$primary_ns->setAttribute ('ref', $path_prefix.$primary_name.'.html');
		$typecat_root->appendChild ($primary_ns);
	    }
	}

	my $parents = ct_get_parents ();
	for my $p (@$parents) {
	    my $duplicate;
	    for my $child ($new_ns->getChildNodes ()) {
		next if not $child->getNodeType == ELEMENT_NODE or
			not $child->getTagName () eq 'parent';

		if ($child->getAttribute ('type') eq $p->getAttribute ('type')) {
		    $duplicate = 1;
		    last;
		}
	    }

	    $new_ns->appendChild ($p) if not $duplicate;
	}

	push_ns ($new_ns);
    } else {
	my $class_type_node = get_type_node (get_ns (), \@class_name_arr);
	if (not $class_type_node) {
	    print STDERR "1 No such type in the type catalogue: ", $name_str, "\n";
	    exit -1;
	}

	my $class = enforce_type_mapping ($class_type_node);
	ct_parentize_children ($class);

	my $category = get_category ();
	if ($category) {
	    my $cat_class = enforce_category_mapping ($category, $class_type_node);
	    ct_parentize_children_copy ($cat_class);
	}

	my $name = $doc->createElement ('name');
	$name->addText ($class_name_arr [$#class_name_arr]);
	$class->appendChild ($name);

	if (ct_get_is_template ()) {
	    my $template = $doc->createElement ('template');
	    $class->appendChild ($template);
	}

	my $ns = ensure_ns_mapping ($name_str);
	if (not $ns) {
	    print STDERR "2 No such type in the type catalogue: ", $name_str, "\n";
	    exit -1;
	}

	push_ns ($ns);
    }

    if ($got_end) {
	pop_popmark ();
    }
}

sub parse_method {
    my $args = shift @_;
    my $body = shift @_;

    my %toplevel;
    push_toplevel (\%toplevel);
    ct_set_decl_handler (\&parse_method_decl);

    if ($args =~ /(~?[\w:]+)/) {
	ct_set_name ($1);
    }

    parse_body (\$body);

    my $name_str = ct_get_name ();
    my @method_name_arr;
    my $method_name;
    printf STDERR "--- NAME: $name_str\n";
    if ($name_str =~ /(?:^|\s)operator\s/ or
	$name_str =~ /^\s*\(/)
    {
	printf STDERR "--- FORCE NAME: $name_str\n";
	$method_name = $name_str;
    } else {
	@method_name_arr = split_type_name ($name_str);
	$method_name = $method_name_arr [$#method_name_arr];
	$#method_name_arr --;
    }

    my $method = $doc->createElement ('method');
    ct_parentize_children ($method);

    my $name = $doc->createElement ('name');
    $method->appendChild ($name);
    $name->addText ($method_name);

    my $ns;
    if (scalar (@method_name_arr)) {
	$ns = get_type_node (get_ns (), \@method_name_arr);
	if (not $ns) {
	    print STDERR "No such type in the type catalogue: ", $name_str, "\n";
	    exit -1;
	}
    } else {
	$ns = get_ns ();
    }

    my $toplevel = enforce_type_mapping ($ns);
    $toplevel->appendChild ($method);

    add_global_to_category ($ns, $method);

    pop_toplevel ();
}

sub parse_type_member {
    my $args = shift @_;
    my $body = shift @_;

    my %toplevel;
    push_toplevel (\%toplevel);
    ct_set_decl_handler (\&parse_type_member_decl);

    if ($args =~ /([\w:]+)/) {
	ct_set_name ($1);
    }

    parse_body (\$body);

    my $name_str = ct_get_name ();
    $name_str =~ /^\s*([&\*]*)/s;
    my $name_prefix = $1;
# FIXME Duplicate lines?
    $name_str =~ s/^\s*[&\*]*//s;
    $name_str =~ s/^\s*[&\*]*//s;
    my @type_name_arr = split_type_name ($name_str);
    my $type_name = $type_name_arr [$#type_name_arr];
    $#type_name_arr --;

    if ($cat_mode) {
	my $parent;
	if (scalar (@type_name_arr)) {
	    $parent = lookup_type_node (get_ns (), \@type_name_arr);
	} else {
	    $parent = get_ns ();
	}

	$parent = $typecat_root if not $parent;

	my $new_ns = $typecat->createElement ('typedef');
	$new_ns->setAttribute ('name', $type_name);
	$parent->appendChild ($new_ns);

	my $typename_prefix;
	if (ns_is_namespace ($parent)) {
	  # For global types, we set 'ref' to category's filename,
	  # or index.html (if there's no category).

	    my $category = get_category ();
	    if ($category) {
		$typename_prefix = get_category_name ($category);
	    } else {
		$typename_prefix = 'index';
	    }
	} else {
	    $typename_prefix = get_typename_prefix ($parent);
	}

	# TODO Generate a valid anchor
	my $anchor_str = $type_name;
	$new_ns->setAttribute ('ref', $path_prefix.$typename_prefix.'.html'."#$anchor_str");
    } else {
	my $type = $doc->createElement ('type_member');
	ct_parentize_children ($type);

	my $name = $doc->createElement ('name');
	$type->appendChild ($name);
	$name->addText ($name_prefix.$type_name);

	# Note: 'anchor' child element seems to be unused.
	my $anchor = $doc->createElement ('anchor');
	$type->appendChild ($anchor);
	# TODO Generate a valid anchor
	$anchor->addText ($type_name);

	my $ns;
	if (scalar (@type_name_arr)) {
	    $ns = get_type_node (get_ns (), \@type_name_arr);
	    if (not $ns) {
		print STDERR "No such type in the type catalogue: ", $name_str, "\n";
		exit -1;
	    }
	} else {
	    $ns = get_ns ();
	}

	my $toplevel = enforce_type_mapping ($ns);
	$toplevel->appendChild ($type);

	add_global_to_category ($ns, $type);
    }

    pop_toplevel ();
}

sub parse_data {
    my $args = shift @_;
    my $body = shift @_;

    my %toplevel;
    push_toplevel (\%toplevel);
    ct_set_decl_handler (\&parse_data_decl);

    if ($args =~ /^\s*([\w:]+)/) {
	ct_set_name ($1);
    }

    parse_body (\$body);

    my $name_str = ct_get_name ();
    print STDERR "--- name: $name_str\n";
    $name_str =~ /^\s*([&\*]*)/s;
    my $name_prefix = $1;
    $name_str =~ s/^\s*[&\*]*//s;
    my @data_name_arr = split_type_name ($name_str);
    my $data_name = $data_name_arr [$#data_name_arr];
    $#data_name_arr --;

    my $data = $doc->createElement ('data');
    ct_parentize_children ($data);

    my $name = $doc->createElement ('name');
    $data->appendChild ($name);
    $name->addText ($name_prefix.$data_name);

    my $ns;
    if (scalar (@data_name_arr)) {
	$ns = get_type_node (get_ns (), \@data_name_arr);
	if (not $ns) {
	    print STDERR "No such type in the type catalogue: ", $name_str, "\n";
	    exit -1;
	}
    } else {
	$ns = get_ns ();
    }

    my $toplevel = enforce_type_mapping ($ns);
    $toplevel->appendChild ($data);

    add_global_to_category ($ns, $data);

    pop_toplevel ();
}

sub get_category_name {
    my $category = shift @_;

    for my $child ($category->getChildNodes ()) {
	if ($child->getNodeType () == ELEMENT_NODE and
	    $child->getTagName () eq 'name')
	{
	    for my $subchild ($child->getChildNodes ()) {
		if ($subchild->getNodeType () == TEXT_NODE) {
		    return $subchild->getNodeValue ();
		}
	    }
	}
    }

  # We shouldn't ever get here.
    return '';
}

sub enforce_category {
    my $name = shift @_;
    my $tag_name = shift @_;

    for my $node ($root->getChildNodes ()) {
	next if not $node->getNodeTypeName () eq 'ELEMENT_NODE';
	next if not ($node->getTagName () eq 'root' or $node->getTagName () eq 'category');

	for my $child ($node->getChildNodes ()) {
	    my $cat_name;

	    if ($child->getNodeType () == ELEMENT_NODE and
		$child->getTagName () eq 'name')
	    {
		for my $subchild ($child->getChildNodes ()) {
		    if ($subchild->getNodeType () == TEXT_NODE) {
			$cat_name = $subchild->getNodeValue ();
			last;
		    }
		}
	    }

	    if ($cat_name eq $name) {
		return $node;
	    }
	}
    }

    return $doc->createElement ($tag_name);
}

# Used only in non-cat mode.
sub parse_category {
    my $args = shift @_;
    my $body = shift @_;

    push_popmark__category ();

#    my %category;
#    push_category (\%category);

    my %toplevel;
    push_toplevel (\%toplevel);

#    parse_desc ('', $body, 3);
    parse_body (\$body);

    my $name_str = $args;

# Deprecated   my $category = $doc->createElement ('category');
    my $category = enforce_category ($name_str, 'category');
    ct_parentize_children ($category);
    push_category ($category);

    my $name = $doc->createElement ('name');
    $category->appendChild ($name);
    $name->addText ($name_str);

    my $filename = $doc->createElement ('filename');
    $category->appendChild ($filename);
    $filename->addText ($name_str.'.html');

    $root->appendChild ($category);

#    pop_popmark ();
}

# Note: this is too similar to parse_category()
sub parse_root {
    my $args = shift @_;
    my $body = shift @_;

    push_popmark__category ();

#    my %category;
#    push_category (\%category);

    my %toplevel;
    push_toplevel (\%toplevel);

#    parse_desc ('', $body, 3);
    parse_body (\$body);

    my $name_str = $args ? $args : "root";

# Deprecated    my $root_el = $doc->createElement ('root');
    my $root_el = enforce_category ($name_str, 'root');
    ct_parentize_children ($root_el);
    push_category ($root_el);

    my $name = $doc->createElement ('name');
    $root_el->appendChild ($name);
    $name->addText ($name_str);

    my $filename = $doc->createElement ('filename');
    $root_el->appendChild ($filename);
    $filename->addText ('root.html');

    $root->appendChild ($root_el);

#    pop_popmark ();
}

sub parse_block {
    my $block = shift @_;

    # Possible variants:
    # a) .class NAME
    # b) .class
    #    .name NAME
    # c) .class
    #    .decl
    #        class NAME {
    if ($block =~ /^\s*\.(global|namespace|class|struct|union|enum)(?:\s+([\w:]+))?[ \t]*$/mg) {
	my $block_type = $1;
	my $block_args = $2;

	my $block_body = substr ($block, pos ($block));
	parse_class ($block_args, $block_body, $block_type);
    } elsif (not $cat_mode and
	     $block =~ /^\s*\.method(?:\s+([\w:]+))?[ \t]*(?=\n|$)/sgc)
    {
	my $args = $1;

	my $body = substr ($block, pos ($block));
	parse_method ($args, $body);
    } elsif ($block =~ /^\s*\.type_member(?:\s+([\w:]+))?[ \t]*(?=\n|$)/sgc) {
	my $args = $1;

	my $body = substr ($block, pos ($block));
	parse_type_member ($args, $body);
    } elsif ($block =~ /^\s*\.end\s*.*$/s) {
	pop_popmark ();
	return;
    } elsif ($block =~ /^\s*\.open(?:\s+|$)/s) {
	$cur_level ++;
    } elsif ($block =~ /^\s*\.close(?:\s+|$)/s) {
	$cur_level -- if $cur_level > 0;

	my $i = 0;
	while ($i < @usings) {
	    if ($usings [$i][0] > $cur_level) {
		if ($i != $#usings ) {
		    $usings [$i] = $usings [$#usings];
		    $#usings --;
		    next;
		} else {
		    $#usings --;
		    last;
		}
	    }

	    $i ++;
	}
    } elsif ($block =~ /^\s*\.eof(?:\s+|$)/s) {
	# TODO Reset usings

	@popmark_stack = ();
	@category_stack = ();
	@toplevel_stack = ();

	# Preserving the last element.
	$#ns_stack = 0;
    } elsif ($block =~ /^\s*\.using\s+([\w:]+)/s) {
	my $type_name = $1;

	my @type_arr = split_type_name ($type_name);
	my $type_node = get_type_node (get_ns (), \@type_arr);
	if ($type_node) {
	    my @using = ($cur_level, $type_node);
	    $usings [@usings] = \@using;
	}
    } elsif ($block =~ /^\s*\.cat(?:\s+([^\.\s].*?))?\s*$/smgc)
    {
	my $args = $1;

	my $body = substr ($block, pos ($block));
	parse_category ($args, $body);
    } elsif ($block =~ /^\s*\.root(?:\s+([^\.\s].*?))?\s*$/smgc)
    {
	my $args = $1;

	my $body = substr ($block, pos ($block));
	parse_root ($args, $body);
    } elsif (not $cat_mode and
	     $block =~ /^\s*\.data(?:\s+([\w]+))?\s*$/smgc)
    {
	my $args = $1;

	my $body = substr ($block, pos ($block));
	parse_data ($args, $body);
    } elsif (not $cat_mode and
	     $block =~ /^\s*\.rdata(?:\s+([^\.\s].*?))?\s*$/smgc)
    {
	my $body = $block;
	$body =~ s/^\s*[^\s]+//;

	if ($body =~ /[^\s]+/s) {
	    parse_data ('', $body);
	}
    }
# TODO Enable this via a command-line option
#      (erdoc comments default to 'data'.
#
#    } elsif (not $cat_mode) {
#	my $body = $block;
#	$body =~ s/^\s*[^\s]+//;
#
#	if ($body =~ /[^\s]+/s) {
#	    parse_data ('', $body);
#	}
#    }
}

binmode (STDOUT, ":utf8");

# Reading erdoc blocks one by one and parsing them.
# Contents of erdoc block is everything before an .eob mark.
my $line;
my $block;
while ($line = <STDIN>) {
    if ($line =~ /^\s*\.eob\s$/) {
	parse_block ($block);
	$block = '';
    } else {
	$block .= $line;
    }
}

if ($cat_mode) {
    print $typecat_root->toString ();
} else {
    postprocess_derivatives ();

    print $xml_pi->toString ();
#    print $xml_pi_xsl->toString;
    print $root->toString ();
}

