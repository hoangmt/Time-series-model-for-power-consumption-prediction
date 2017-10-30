# The script MUST contain a function named azureml_main
# which is the entry point for this module.
import requests
from requests.packages.urllib3.exceptions import InsecureRequestWarning
requests.packages.urllib3.disable_warnings(InsecureRequestWarning)
import pandas as pd
import numpy as np
import datetime
#from sklearn import ensemble
try:
   import cPickle as pickle
except:
   import pickle
#import gzib
import base64
import zlib
from azure.storage.blob import BlockBlobService
import time
from datetime import timedelta
#from sklearn.preprocessing import StandardScaler
#from sklearn.svm import SVR
#from sklearn import linear_model
#from sklearn import metrics
print('in substation train')
from processing.substation import Substation
from common.exceptions import AssetNotFoundError

import subprocess
class SubstationTrain(Substation):
    def __init__(self):
        print('init substation train')
        Substation.__init__(self)
        print('finish substation train')


    def train(self, First_Training_Date,Last_Training_Date, substation_id,nahead):
        import sys
        print('begin train python')
        import os

        if not self.is_valid_id(substation_id):
            raise AssetNotFoundError(substation_id)
        # Define command and arguments
        print('inside train python')
        path2script = os.path.join(os.getcwd(),'substation','Rfunc.R')
        print(path2script)
        #path2script = 'r_func.R'
        command = 'Rscript'

        #args = [2, substation_id, from_datetime, nahead]  # 1 means prediction
        args = ['1',substation_id, First_Training_Date, Last_Training_Date,nahead]
        cmd = [command, path2script] + args
        print(cmd)
        # check_output will run the command and store to result
        #a=subprocess.check_output(cmd, universal_newlines=True)
        #print(a)

        with subprocess.Popen(cmd, stdout=subprocess.PIPE, bufsize=1, universal_newlines=True) as p:
            for line in p.stdout:
                print(line, end='')

        #output = p.communicate()[0]
        #exitCode = p.returncode
        print('finish train python')
