from setuptools import setup, find_packages

setup (
  name='janus_lexer',
  packages=find_packages(),
  entry_points =
  """
 [pygments.lexers]
 janus = januslexer:JanusLexer
 """,
)
