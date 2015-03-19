package sxr
package output
package html

import scala.tools.nsc.doc

abstract class HtmlPage extends doc.html.HtmlPage {
/*
 *         <script type="text/javascript" src="jquery-all.js"></script>
        <script type="text/javascript" src="linked.js"></script>
        <link rel="stylesheet" type="text/css" href="style.css" title="Style"></link>
 */
  
  
  val tpl = 
    <html>
      <head>
        <title>{ title }</title>
        <meta charset="utf-8"/>
        <meta name="description" content={ description }/>
        <script>exports = this</script>
        { headers }

      </head>
      <body class="literate">
        <article id="documentation">
          { body }
        </article>
      </body>
    </html>
  
}