from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import datetime  # For datetime objects
import os.path  # To manage paths
import sys  # To find out the script name (in argv[0])
import pandas as pd
from tqdm import tqdm
# Import the backtrader platform
import backtrader as bt
from Portfolio import TestStrategy, MySizer

# Create a Stratey
port_num = 20


def getStocks():
    table = pd.read_html(
        'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies')
    df = table[0]
    df.to_csv('S&P500-Info.csv')
    df.to_csv("S&P500-Symbols.csv", columns=['Symbol'])
    tickers = df['Symbol'][:port_num]
    result = []
    tick_map = {}
    print("Getting historical stock data")
    for tick in tqdm(tickers):
        data = bt.feeds.YahooFinanceData(
            dataname=tick,
            # Do not pass values before this date
            fromdate=datetime.datetime(2019, 1, 1),
            # Do not pass values after this date
            todate=datetime.datetime(2020, 12, 31),
            reverse=False)
        tick_map[tick] = len(result)
        result.append(data)
    return (tick_map, result)


if __name__ == '__main__':
    # Create a cerebro entity
    cerebro = bt.Cerebro()

    # Add a strategy
    cerebro.addstrategy(TestStrategy, port_num=port_num)

    # Create a Data Feed
    data = bt.feeds.YahooFinanceData(
        dataname="T",
        # Do not pass values before this date
        fromdate=datetime.datetime(2019, 1, 1),
        # Do not pass values after this date
        todate=datetime.datetime(2020, 12, 31),
        reverse=False)
    # Add the Data Feed to Cerebro
    cerebro.adddata(data)
    # tickers, stocks = getStocks()
    # for data in stocks:
    #     cerebro.adddata(data)

    # Set our desired cash start
    cerebro.broker.setcash(1000.0)

    # Add a FixedSize sizer according to the stake
    # cerebro.addsizer(bt.sizers.FixedSize, stake=10)
    # cerebro.addsizer(MySizer)
    cerebro.addsizer(bt.sizers.PercentSizer, percents=10)

    # Set the commission
    cerebro.broker.setcommission(commission=0.0)

    # Print out the starting conditions
    print('Starting Portfolio Value: %.2f' % cerebro.broker.getvalue())

    # Run over everything
    cerebro.run()

    # Print out the final result
    print('Final Portfolio Value: %.2f' % cerebro.broker.getvalue())

    # Plot the result
    cerebro.plot()
