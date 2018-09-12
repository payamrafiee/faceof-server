import mongoose, { Schema } from 'mongoose';

const UserSchema = new Schema({
  phoneNum: {
    type: String,
    trim: true,
    unique: true,
    required: [true, 'Phone number is required!'],
  },
});

export default mongoose.model('User', UserSchema);
