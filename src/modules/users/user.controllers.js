import User from './user.model';

export async function signup(req, res) {
  try {
    const user = await User.create({ ...req.body });
    return res.send(user);
  } catch (e) {
    return res.status(500).json(e);
  }
}

export async function login(req, res, next) {
  res.status(200).json(req.user);

  return next();
}

