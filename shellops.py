'''
@sonicskye@2020

Shell operation functions

'''

import os

dir_path = os.path.dirname(os.path.realpath(__file__))+"/"
bx_path = dir_path + "bx"

def getbxoutput(command):
    return os.popen(bx_path + " " + str(command)).read().strip()
