from flask_restplus import reqparse
pagination_arguments = reqparse.RequestParser()
print('in request')
request_train = reqparse.RequestParser()
request_train.add_argument('First_Training_Date', type=str, required=True, help='Starting date of the training data, format 2017-06-16T00:00:00')
print('middle request')
request_train.add_argument('Last_Training_Date', type=str, required=True, help='The last date of the training data, format 2017-06-16T00:00:00')
request_train.add_argument('substation_id', type=str, required=True, help='Substation id for training')
request_train.add_argument('nahead', type=str, required=True, help='Number of hours to predict')



request_pred = reqparse.RequestParser()
request_pred.add_argument('date_start_prediction', type=str, required=True, help='Date from which the model will do prediction, format 2017-06-16T00:00:00')
#request_pred.add_argument('date_extract_data', type=str, required=True, help='Date from which we extract data for prediction, format 2017-06-16')
request_pred.add_argument('substation_id', type=str, required=True, help='Substation id for prediction')
request_pred.add_argument('nahead', type=str, required=True, help='Number of hours to predict')