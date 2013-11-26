from distribute_setup import use_setuptools
use_setuptools()
from setuptools import setup

setup(
    name='robusta',
    version='0.1.0dev',
    author='D. Shore',
    author_email='diarmuid.shore@mtm.kuleuven.be',
    packages=['robusta'],
    package_dir={'robusta': 'robusta'},
    package_data={'robusta': ['analysis/*', 'assemble/*',
                  'control/*', 'geom/*', 'material/*',
                  'mesh/*', 'post/*', 'sketch/*',
                  'test/*', 'util/*']},
    license='LICENSE.txt',
    description='Extensions for the Abaqus CAE script interface for building parametric models',
    long_description=open('README.txt').read(),
    install_requires=[
        "numpy >= 1.4.0",
    ],
)
