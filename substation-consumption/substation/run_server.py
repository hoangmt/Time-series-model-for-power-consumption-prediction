import logging
logging.basicConfig(level=logging.DEBUG)
from flask import Flask, Blueprint
from substation import config
from substation.endpoints import ns as processing_namespace
from substation.api import api

app = Flask(__name__)
# log = logging.getLogger(__name__)


def configure_app(flask_app):
    # flask_app.config['SERVER_NAME'] = config.FLASK_SERVER_NAME
    flask_app.config['SWAGGER_UI_DOC_EXPANSION'] = config.RESTPLUS_SWAGGER_UI_DOC_EXPANSION
    flask_app.config['RESTPLUS_VALIDATE'] = config.RESTPLUS_VALIDATE
    flask_app.config['RESTPLUS_MASK_SWAGGER'] = config.RESTPLUS_MASK_SWAGGER
    flask_app.config['ERROR_404_HELP'] = config.RESTPLUS_ERROR_404_HELP
    print('insdie configure_app 2')


def initialize_app(flask_app):
    configure_app(flask_app)
    print('inside initialize_app 1')

    blueprint = Blueprint('substation', __name__, url_prefix='/substation')
    api.init_app(blueprint)
    api.add_namespace(processing_namespace)
    flask_app.register_blueprint(blueprint)


def main():
    print('here')
    initialize_app(app)
    logging.info('Starting development server')
    print('hereeee')
    #app.run(debug=config.FLASK_DEBUG, host='127.0.0.1', port=config.FLASK_PORT)
    app.run(debug=config.FLASK_DEBUG, host='0.0.0.0', port=config.FLASK_PORT)

if __name__ == "__main__":
    main()