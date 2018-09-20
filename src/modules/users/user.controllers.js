import { hashSync, compareSync } from 'bcrypt-nodejs';
import User from './user.model';

const client = require('twilio')('ACdcf9039846ddd1542aee439dff3170c2', '13519f53eed795553b4901a55c1ebee0');

export async function signup(req, res) {
  const code = Math.floor(Math.random() * 9000) + 1000;
  console.log('====================================');
  console.log(code);
  console.log('====================================');
  try {
    // client.messages
    //   .create({
    //     body: `this is your code: ${code}`,
    //     from: '+18509905322',
    //     to: `+${req.body.phone_number}`,
    //   });
    const user = await User.create({ ...req.body, verify_code: code });
    return res.send(user);
  } catch (e) {
    return res.status(500).json(e);
  }
}

export async function login(req, res) {
  try {
    const { phone_number, verify_code } = req.body;
    const user = await User.findOne({ phone_number });

    if (!user) {
      return console.log('user not found');
    }
    if (user.authenticateUser(verify_code)) {
      res.status(200).send('You are successfully loged in');
    } else {
      res.status(400).send('wrong password');
    }
  } catch (error) {
    console.log(error);
  }
}

