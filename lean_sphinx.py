from docutils import nodes
from docutils.parsers.rst import Directive
from sphinx.builders import Builder
from sphinx.directives import CodeBlock
from sphinx.errors import SphinxError
import os, os.path, glob, subprocess
import urllib

try:
    urlquote = urllib.parse.quote
except:
    # Python 2
    def urlquote(s, safe='/'):
        return urllib.quote(s.encode('utf-8'), safe)


# "Try it!" button

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


# Extract code snippets for testing.

class LeanTestBuilder(Builder):
    '''
    Extract ``..code-block:: lean`` directives for testing.
    '''
    name = 'leantest'

    def init(self):
        self.written_files = set()

    def write_doc(self, docname, doctree):
        i = 0
        for node in doctree.traverse(nodes.literal_block):
            if node['language'] != 'lean': continue
            i += 1
            fn = os.path.join(self.outdir, '{0}_{1}.lean'.format(docname, i))
            self.written_files.add(fn)
            out = open(fn, mode='w', encoding='utf-8')
            out.write(node.rawsource)
    
    def finish(self):
        old_files = glob.glob(os.path.join(self.outdir, '**', '*.lean'), recursive=True)
        for fn in old_files:
            if fn not in self.written_files:
                os.remove(fn)       

        output = subprocess.run(['lean', '--make', self.outdir], stdout=subprocess.PIPE)
        errors = '\n'.join(l for l in output.stdout.decode('utf-8').split('\n') if ': error:' in l)
        if errors != '': raise SphinxError('\nlean exited with errors:\n{0}\n'.format(errors))
        output.check_returncode()

    def prepare_writing(self, docnames): pass

    def get_target_uri(self, docname, typ=None):
        return ''

    def get_outdated_docs(self):
        return self.env.found_docs

def setup(app):
    app.add_node(lean_code_goodies,
        html=(html_visit_lean_code_goodies, html_depart_lean_code_goodies))
    app.connect('doctree-resolved', process_lean_nodes)

    app.add_builder(LeanTestBuilder)

    return {'version': '0.1'}
