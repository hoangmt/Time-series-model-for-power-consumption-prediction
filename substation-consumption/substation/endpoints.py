import logging
import sys

from flask import request
from flask_restplus import Resource

from common.exceptions import AssetNotFoundError
from common.timeseries import TimeSeriesSeries, TimeSeriesFlaskModel
from common.request import request_train, request_pred
print('in endpoints')
from substation.processing.substation_train import SubstationTrain
from substation.processing.substation_pred import SubstationPred
from substation.api import api

ns = api.namespace('substation', description='Processing of Substation service')
ts_model = TimeSeriesFlaskModel(api)
ts_series = ts_model.ts_series


@ns.route('/train')
@api.response(404, 'Asset Id not found')
class TrainEndpoint(Resource):
    @api.expect(request_train, validate=True)
    @api.response(201, 'Model successfully updated.')
    def get(self):
        """
        Train prosumer model for this Asset id.
        """
        print('begining of Train End point')
        print(request)
        args = request_train.parse_args(request)
        print('1')
        logging.debug('Receiving request arguments {}'.format(args))
        Last_Training_Date = args.get('Last_Training_Date', '0000-00-00T00:00:00')
        print('2')
        First_Training_Date = args.get('First_Training_Date', '0000-00-00T00:00:00')
        nahead = args.get('nahead')
        substation_id = args.get('substation_id')
        print('inside TrainEndPoint', substation_id)
        self.train = SubstationTrain()
        model = self.train
        print('inside TrainEndPoint 2')
        model.train(First_Training_Date, Last_Training_Date, substation_id,nahead)
        print('After Training')
        logging.debug('Successfully trained substation model')
        return None, 201


@ns.route('/predict')
@api.response(404, 'Asset Id not found')
class PredEndpoint(Resource):
    @api.expect(request_pred, validate=True)
    @api.marshal_with(ts_series)
    @api.response(200, 'Success')
    def get(self):
        """
        Return result predicted from substation model.
        """
        args = request_pred.parse_args(request)
        logging.debug('Receiving request arguments {}'.format(args))
        date_start_prediction = args.get('date_start_prediction', '0000-00-00T00:00:00')
        #date_from = args.get('date_from', '0000-00-00T00:00:00')
        print('date_start_prediction')
        print(date_start_prediction)
        #date_extract_data = args.get('date_extract_data', '0000-00-00')
        substation_id = args.get('substation_id')
        nahead = args.get('nahead')
        print('Number of hours to predict')
        model = SubstationPred()
        df_res = model.predict(date_start_prediction, substation_id,nahead)
        res = model.convert_ts_model(df_res,date_start_prediction)
        logging.debug('Successfully performing prediction for substation')
        return res,200
