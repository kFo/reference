from docutils import nodes
from docutils.parsers.rst import Directive
from sphinx.directives import CodeBlock
import urllib

try:
    urlquote = urllib.parse.quote
except:
    # Python 2
    def urlquote(s, safe='/'):
        return urllib.quote(s.encode('utf-8'), safe)

class lean_code_goodies(nodes.General, nodes.Element): pass

def mk_try_it_uri(code):
    uri = 'https://gebner.github.io/lean-web-editor/#code='
    uri += urlquote(code, safe='~@#$&()*!+=:;,.?/\'')
    return uri

def process_lean_nodes(app, doctree, fromdocname):
    env = app.builder.env
    for node in doctree.traverse(nodes.literal_block):
        if node['language'] != 'lean': continue

        new_node = lean_code_goodies()
        new_node['full_code'] = node.rawsource
        node.replace_self([new_node])
        new_node += node

        # TODO(gabriel): remove code outside BEGIN/END

def html_visit_lean_code_goodies(self, node):
    self.body.append(self.starttag(node, 'div', style='position: relative'))
    self.body.append("<div style='position: absolute; right: 0; top: 0; padding: 1ex'>")
    self.body.append(self.starttag(node, 'a', target='_blank', href=mk_try_it_uri(node['full_code'])))
    self.body.append('try it!</a></div>')

def html_depart_lean_code_goodies(self, node):
    self.body.append('</div>')

def setup(app):
    app.add_node(lean_code_goodies,
        html=(html_visit_lean_code_goodies, html_depart_lean_code_goodies))
    app.connect('doctree-resolved', process_lean_nodes)

    return {'version': '0.1'}
