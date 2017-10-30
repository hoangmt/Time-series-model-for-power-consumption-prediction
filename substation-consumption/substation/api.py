import logging
import traceback
from flask_restplus import Api
from substation import config
from common.exceptions import AssetNotFoundError

api = Api(version='1.0', title='Substation Services API',
          description='An API for training and performing prediction with Prosumer models')


@api.errorhandler
def default_error_handler(e):
    message = 'An unhandled exception occurred.'
    logging.error(message)

    if not config.FLASK_DEBUG:
        return {'message': message}, 500


@api.errorhandler(AssetNotFoundError)
def asset_not_found_handler(e):
    logging.error(traceback.format_exc())
    return {'message': 'Asset Id out of pool'}, 404