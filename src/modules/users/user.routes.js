import { Router } from 'express';
import validate from 'express-validation';

import * as userController from './user.controllers';
import userValidation from './user.validations';

const routes = new Router();

routes.post('/signup', validate(userValidation.signup), userController.signup);
routes.post('/login', userController.login);

export default routes;
