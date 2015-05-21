#!/usr/bin/env python

from setuptools import setup

setup(name='features_extraction',
      version='1.0',
      description='Speech features extraction in python',
      author='Roland Thiolliere',
      author_email='rolthiolliere@gmail.com',
      packages=[],
      license='licence/LICENSE.txt',
      long_description=open('README.md').read(),
      dependency_links = ['http://github.com/bootphon/spectral/tarball/master/#egg=mwv-spectral'],
      install_requires=[
          "mwv-spectral",
          "h5features",
      ],
  )
