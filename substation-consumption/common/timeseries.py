from flask_restplus import fields


class TimeSeriesFlaskModel:

    def __init__(self, api):
        """        
        :param api: type of flask_restplus.Api 
        """
        self.ts_item = api.model('Value of one temporal item', {
            'time': fields.String(description='A particular time', default='2017-04-06T09:05:00'),
            'value': fields.Float(description='Value for this timing point', default=0.95)
        })

        self.ts_series = api.model('Values of a series of timing points', {
            'series': fields.List(fields.Nested(self.ts_item))
        })


class TimeSeriesItem:
    def __init__(self, time, value):
        self.time = time
        self.value = value


class TimeSeriesSeries:
    def __init__(self, series=None):
        self.series = series

    def from_items(self, times, values):
        """
        Form a series from list of times and corresponding values
        :param times: 
        :param values: 
        :return: a series
        """
        self.series = list(map(lambda t, v: TimeSeriesItem(t, v), times, values))
