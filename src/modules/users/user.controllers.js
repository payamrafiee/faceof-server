import User from './user.model';

export async function signup(req, res) {
  try {
    const user = await User.create({ ...req.body });
    return res.send(user);
  } catch (e) {
    return res.status(500).json(e);
  }
}

export async function login(req, res) {
  try {
    const { email, password } = req.body;
    const user = await User.findOne({ email });

    if (!user) {
      return console.log('user not found');
    }
    if (user.authenticateUser(password)) {
      res.status(200).send('You are successfully loged in');
    } else {
      res.status(400).send('wrong password');
    }
  } catch (error) {
    console.log(error);
  }
}

