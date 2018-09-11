import express from 'express';

import constants from './config/constants';
import middlewaresConfig from './config/middlewares';
import './config/database';

const app = express();

middlewaresConfig(app);

app.get('/', (req, res) => {
  res.send('hello world');
});

app.listen(constants.PORT, err => {
  if (err) {
    console.log(err);
  } else {
    console.log(`
      Server running on port: ${constants.PORT}
      ------
      Running on ${process.env.NODE_ENV}
    `);
  }
});
