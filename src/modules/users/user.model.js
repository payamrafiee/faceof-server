import mongoose, { Schema } from 'mongoose';
import { hashSync, compareSync } from 'bcrypt-nodejs';

const UserSchema = new Schema({
  phone_number: {
    type: String,
    trim: true,
    unique: true,
    required: [true, 'Phone number is required!'],
  },
  verify_code: {
    type: String,
  },
  phone_status: {
    type: String,
  },
});

UserSchema.pre('save', function (next) {
  if (this.isModified('verify_code')) {
    this.verify_code = this._hashCode(this.verify_code);
  }
  return next();
});

UserSchema.methods = {
  _hashCode(code) {
    return hashSync(code);
  },
  authenticateUser(code) {
    return compareSync(code, this.verify_code);
  },
};

export default mongoose.model('User', UserSchema);
