import logging
import os
import pandas as pd
from common.timeseries import TimeSeriesSeries
#Call prosumer?train and predict
print('in substation')
dir_path = os.path.dirname(os.path.realpath(__file__))
ASSET_FILE = os.path.join(dir_path, 'substation_relations.csv')

class Substation:
    def __init__(self, asset_file=ASSET_FILE):
        self.asset_file = asset_file
        print(asset_file)
        #print(list(pd.read_csv(asset_file)['Substation']))

        self.substation_id_pool = list(pd.read_csv(asset_file,encoding='latin-1')['SubstationId'])
        #print(sef.substation_id_pool)
        print('inside substation init')
        #print(list(pd.read_csv(asset_file,encoding='latin-1')['SubstationId']))
        logging.debug('There are {} asset devices'.format(len(self.substation_id_pool)))

    def is_valid_id(self, substation_id):
        return int(substation_id) in self.substation_id_pool

    @staticmethod
    def convert_ts_model(df_ts,date_start_prediction):
        """
        Convert to TimeSeriesSeries
        :param df_ts: DataFrame which is indexed by dates and has only one column of values          
        """
        #indexes = list(map(lambda x: str(x), df_ts.index.values))
        from datetime import datetime
        from datetime import timedelta
        #indexes = list(map(lambda x: str(x), date_start_prediction + df_ts.index.values))
        values = df_ts.ix[:, 0].values
        print('insider convert ts')
        print(values)
        nahead=len(values)
        indexes=[]
        for i in range(1,nahead+1):
            indexes.append(str(datetime.strptime(date_start_prediction, '%Y-%m-%dT%H:%M:%S')+timedelta(hours=i)))
        print(indexes)
        ts_model = TimeSeriesSeries()
        ts_model.from_items(indexes, values)
        return ts_model
