import mongoose, { Schema } from 'mongoose';
import validator from 'validator';
import { hashSync, compareSync } from 'bcrypt-nodejs';
import jwt from 'jsonwebtoken';

import { passwordReg } from './user.validations';
import constants from '../../config/constants';

const UserSchema = new Schema({
  email: {
    type: String,
    trim: true,
    unique: true,
    required: [true, 'Phone number is required!'],
    validate: {
      validator(email) {
        return validator.isEmail(email);
      },
      message: '{VALUE} is not a valid email!',
    },
  },
  password: {
    type: String,
    trim: true,
    required: [true, 'Password is required!'],
    minlength: [6, 'Password need to be longer!'],
    validate: {
      validator(password) {
        return passwordReg.test(password);
      },
      message: '{VALUE} is not a valid password!',
    },
  },
});

UserSchema.pre('save', function (next) {
  if (this.isModified('password')) {
    this.password = this._hashPassword(this.password);
  }
  return next();
});

UserSchema.methods = {
  _hashPassword(password) {
    return hashSync(password);
  },
  authenticateUser(password) {
    return compareSync(password, this.password);
  },
  createToken() {
    return jwt.sign(
      {
        _id: this._id,
      },
      constants.JWT_SECRET,
    );
  },
  toJSON() {
    return {
      _id: this._id,
      token: `JWT ${this.createToken()}`,
    };
  },
};

export default mongoose.model('User', UserSchema);
