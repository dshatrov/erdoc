<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:exslt="http://exslt.org/common"
		extension-element-prefixes="exslt">
<xsl:output method="xml"
	    version="1.0"
	    encoding="UTF-8"
	    omit-xml-declaration="yes"/>

<xsl:template match="merge">
	<ertypes>
		<xsl:apply-templates/>
	</ertypes>
</xsl:template>

<xsl:template match="*">
	<xsl:copy-of select="./*"/>
</xsl:template>

</xsl:stylesheet>

