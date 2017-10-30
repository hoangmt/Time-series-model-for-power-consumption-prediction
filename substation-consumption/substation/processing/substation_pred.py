# The script MUST contain a function named azureml_main
# which is the entry point for this module.

# imports up here can be used to 
import requests
import pandas as pd
import numpy as np
import logging
try:
   import cPickle as pickle
except:
   import pickle
from azure.storage.blob import BlockBlobService
import base64
import zlib
# The entry point function can contain up to two input arguments:
#   Param<dataframe1>: a pandas.DataFrame
#   Param<dataframe2>: a pandas.DataFrame

from processing.substation import Substation
from common.exceptions import AssetNotFoundError

import subprocess
class SubstationPred(Substation):
    def __init__(self):
        Substation.__init__(self)

    def predict(self, from_datetime,substation_id,nahead):
        import os
        if not self.is_valid_id(substation_id):
            raise AssetNotFoundError(substation_id)
        logging.debug('Start predicting process ...')
        print('inside prediction')
        logging.debug('Done prediction and converted to DataFrame')
        #return predictions
        print('inside train python')
        #path2script = os.getcwd() + '\substation\substation_pred.R'
        path2script = os.path.join(os.getcwd(), 'substation', 'Rfunc.R')
        print(path2script)
        # path2script = 'r_func.R'
        command = 'Rscript'
        args = ['2',substation_id, from_datetime,nahead]#2 means prediction
        cmd = [command, path2script] + args
        print(cmd)
        # check_output will run the command and store to result
        # a=subprocess.check_output(cmd, universal_newlines=True)
        # print(a)

        #p=subprocess.Popen(cmd, stdout=subprocess.PIPE, bufsize=1, universal_newlines=True)

        #output = subprocess.Popen(cmd, shell=False, stdout=subprocess.PIPE,stderr=subprocess.PIPE)
        #final = output.stdout.read()

        with subprocess.Popen(cmd, stdout=subprocess.PIPE, bufsize=1, universal_newlines=True) as p:
             #print('Length of p is',len(p.stdout))
            for line in p.stdout:
                print(line, end='')  # output = p.communicate()[0]
                # exitCode = p.returncode
        print('finish predicting in python---------------------------------')
        #print('here',final)
        path2datafile = os.path.join(os.getcwd(), 'substation', 'data','y_pred.csv')
        import pandas as pd
        y_pred = pd.read_csv(path2datafile)
        print('finish predicting in python---------------------------------')
        return (y_pred)