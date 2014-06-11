#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse

def main():
    parser = argparse.ArgumentParser(description="Legolas batch load.")
    parser.add_argument(
            '--rootdir', '-d',
            help='')
    parser.add_argument(
            '--threads',
            help='Load threads',
            type=int,
            default=5)
    args = parser.parse_args()
    print args
    #print args.rootdir

    if args.rootdir is None:
        print "Root dir is None."
        exit

if __name__ == '__main__':
    main()

